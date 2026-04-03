use std::fs::File;
use std::io::{BufWriter, Write};

use bytes_ext::U16Ext;

use crate::address::{Address, addr};
use crate::bios::Bios;
use crate::clock::{Attoseconds, Clock};
use crate::cpu::{Callback, Cpu, CpuContext};
use crate::device::Device;
use crate::device::keyboard::Keyboard;
use crate::device::opl3::OPL3;
use crate::device::pit::Pit;
use crate::device::vga::Vga;
use crate::dos::Dos;
use crate::file_system::FileSystemManager;
use crate::frame::{FrameCacheSync, FrameSender};
use crate::input_event::{InputEvent, InputEventReceiver};
use crate::memory::Memory;
use crate::palette::Pal888;

pub struct Machine {
    pub bios: Box<Bios>,
    pub clock: Clock,
    pub cpu: Box<Cpu>,
    pub devices: Devices,
    pub dos: Box<Dos>,
    pub file_system_manager: FileSystemManager,
    pub memory: Memory,
    pub input_event_receiver: Option<InputEventReceiver>,
}

pub struct Devices {
    pub keyboard_device_id: usize,
    pub pit_device_id: usize,
    pub vga_device_id: usize,
    pub devices: DeviceList,
    pub io_map: Vec<IoMapEntry>,
}

type IoMapEntry = ((u16, u16), usize);

impl Devices {
    pub fn iter(&mut self) -> impl Iterator<Item = &Box<dyn Device>> {
        self.devices.0.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Box<dyn Device>> {
        self.devices.0.iter_mut()
    }

    pub fn get(&self, id: usize) -> &dyn Device {
        self.devices
            .0
            .get(id)
            .expect("Device ID out of bounds")
            .as_ref()
    }

    pub fn get_mut(&mut self, id: usize) -> &mut dyn Device {
        self.devices
            .0
            .get_mut(id)
            .expect("Device ID out of bounds")
            .as_mut()
    }

    pub fn pit(&mut self) -> &mut Pit {
        self.get_mut(self.pit_device_id)
            .as_any_mut()
            .downcast_mut()
            .unwrap()
    }

    pub fn vga(&self) -> &Vga {
        self.get(self.vga_device_id)
            .as_any()
            .downcast_ref()
            .unwrap()
    }

    pub fn vga_mut(&mut self) -> &mut Vga {
        self.get_mut(self.vga_device_id)
            .as_any_mut()
            .downcast_mut()
            .unwrap()
    }

    pub fn keyboard(&mut self) -> &mut Keyboard {
        self.get_mut(self.keyboard_device_id)
            .as_any_mut()
            .downcast_mut()
            .unwrap()
    }
}

impl<'a> IntoIterator for &'a Devices {
    type Item = &'a Box<dyn Device>;

    type IntoIter = std::slice::Iter<'a, Box<dyn Device>>;

    fn into_iter(self) -> Self::IntoIter {
        self.devices.0.iter()
    }
}

impl<'a> IntoIterator for &'a mut Devices {
    type Item = &'a mut Box<dyn Device>;

    type IntoIter = std::slice::IterMut<'a, Box<dyn Device>>;

    fn into_iter(self) -> Self::IntoIter {
        self.devices.0.iter_mut()
    }
}

pub struct DosMachineContext<'a> {
    pub cpu: &'a mut Cpu,
    pub devices: &'a mut Devices,
    pub file_system_manager: &'a mut FileSystemManager,
    pub memory: &'a mut Memory,
}

pub struct CpuMachineContext<'a> {
    pub dos: &'a mut Dos,
    pub devices: &'a mut Devices,
    pub file_system_manager: &'a mut FileSystemManager,
    pub memory: &'a mut Memory,
}

pub struct DeviceMachineContext<'a> {
    pub cpu: &'a mut Cpu,
    pub memory: &'a mut Memory,
}

pub trait MachineContext {
    fn devices(&mut self) -> &mut Devices;
}

impl CpuContext for CpuMachineContext<'_> {
    fn memory(&mut self) -> &mut Memory {
        self.memory
    }

    fn io_read_u8(&mut self, port: u16) -> u8 {
        let devices = self.devices();

        // println!("I/O read at port {port:04X}");

        for ((from, to), id) in devices.io_map.iter().copied() {
            if (from..=to).contains(&port) {
                return devices.get_mut(id).read(port - from);
            }
        }
        // println!("Unimplemented I/O read at port {port:04X}");
        0xff
    }

    fn io_write_u8(&mut self, port: u16, v: u8) {
        let devices = self.devices();

        // println!("I/O write at port {port:04X} with value {v:02X}");

        for ((from, to), id) in devices.io_map.iter().copied() {
            if (from..=to).contains(&port) {
                devices.get_mut(id).write(port - from, v);
                return;
            }
        }
        // println!("Unimplemented I/O write at port {port:04X}");
    }

    fn io_read_u16(&mut self, port: u16) -> u16 {
        let devices = self.devices();

        // println!("I/O read at port {port:04X}");

        for ((from, to), id) in devices.io_map.iter().copied() {
            if (from..=to).contains(&port) {
                let lo = devices.get_mut(id).read(port - from);
                let hi = devices.get_mut(id).read((port - from).wrapping_add(1));
                return ((hi as u16) << 8) + (lo as u16);
            }
        }
        // println!("Unimplemented I/O read at port {port:04X}");
        0xffff
    }

    fn io_write_u16(&mut self, port: u16, v: u16) {
        let devices = self.devices();

        // println!("I/O write at port {port:04X} with value {v:04X}");

        for ((from, to), id) in devices.io_map.iter().copied() {
            if (from..=to).contains(&port) {
                devices.get_mut(id).write(port - from, v.lo());
                devices
                    .get_mut(id)
                    .write((port - from).wrapping_add(1), v.hi());
                return;
            }
        }
        // println!("Unimplemented I/O write at port {port:04X}");
    }

    fn vga(&self) -> &Vga {
        self.devices.vga()
    }
}

impl MachineContext for CpuMachineContext<'_> {
    fn devices(&mut self) -> &mut Devices {
        self.devices
    }
}

impl MachineContext for DosMachineContext<'_> {
    fn devices(&mut self) -> &mut Devices {
        self.devices
    }
}

pub fn install_interrupt_handler_callback(
    cpu: &mut Cpu,
    memory: &mut Memory,
    int_num: u8,
    callback: Callback,
) -> Address {
    let int_table_addr = addr(0x0000, 4 * (int_num as u16));

    let hook_addr = cpu.register_callback(memory, callback);
    memory.write_u16(int_table_addr + 0, hook_addr.ofs);
    memory.write_u16(int_table_addr + 2, hook_addr.seg);

    hook_addr
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

pub struct DeviceList(Vec<Box<dyn Device>>);

impl DeviceList {
    pub fn new() -> Self {
        DeviceList(Vec::new())
    }

    pub fn add(&mut self, device: Box<dyn Device>) -> usize {
        let id = self.0.len();
        self.0.push(device);
        id
    }
}

impl Default for DeviceList {
    fn default() -> Self {
        Self::new()
    }
}

impl Machine {
    pub fn new() -> Self {
        let clock = Clock(14.31818);
        let mut memory = Memory::new();
        let mut cpu = Box::new(Cpu::new(clock / 3));
        let mut dos = Box::new(Dos::new());
        let mut bios = Box::new(Bios::new());
        let file_system_manager = FileSystemManager::new();

        let frame_cache = FrameCacheSync::new();

        let devices = {
            let mut devices = DeviceList::new();

            // let pic_device_id = devices.len();
            // devices.push(Box::new(Pic::new()));

            let keyboard_device_id = devices.add(Box::new(Keyboard::new()));
            let pit_device_id = devices.add(Box::new(Pit::new()));
            let opl3_device_id = devices.add(Box::new(OPL3::new()));
            let vga_device_id = devices.add(Box::new(Vga::new(frame_cache.clone())));

            let io_map = vec![
                // ((0x020, 0x021), pic_device_id),
                ((0x040, 0x059), pit_device_id),
                // 8042 keyboard controller ports
                ((0x060, 0x064), keyboard_device_id),
                ((0x388, 0x38b), opl3_device_id),
                ((0x3c0, 0x3df), vga_device_id),
            ];

            Devices {
                keyboard_device_id,
                pit_device_id,
                vga_device_id,
                devices,
                io_map,
            }
        };

        // Let the BIOS install its callbacks into the CPU
        bios.install(&mut cpu, &mut memory);

        // Let DOS install its callbacks into the CPU
        dos.install(&mut cpu, &mut memory);

        dos.current_directory.extend_from_slice(b"C:\\\0");

        Machine {
            bios,
            clock,
            cpu,
            devices,
            dos,
            file_system_manager,
            memory,
            input_event_receiver: None,
        }
    }

    pub fn set_frame_sender(&mut self, tx: FrameSender) {
        self.devices.vga_mut().set_frame_sender(tx);
    }

    pub fn set_input_receiver(&mut self, rx: InputEventReceiver) {
        self.input_event_receiver = Some(rx);
    }

    pub fn get_dos(&mut self) -> &mut Dos {
        self.dos.as_mut()
    }

    pub fn get_cpu_and_context(&mut self) -> (&mut Cpu, CpuMachineContext<'_>) {
        let cpu_context = CpuMachineContext {
            dos: self.dos.as_mut(),
            devices: &mut self.devices,
            file_system_manager: &mut self.file_system_manager,
            memory: &mut self.memory,
        };
        (&mut self.cpu, cpu_context)
    }

    pub fn get_bios_and_context(&mut self) -> (&mut Bios, DosMachineContext<'_>) {
        let ctx = DosMachineContext {
            cpu: &mut self.cpu,
            devices: &mut self.devices,
            file_system_manager: &mut self.file_system_manager,
            memory: &mut self.memory,
        };
        (self.bios.as_mut(), ctx)
    }

    pub fn get_dos_and_context(&mut self) -> (&mut Dos, DosMachineContext<'_>) {
        let dos_context = DosMachineContext {
            cpu: &mut self.cpu,
            devices: &mut self.devices,
            file_system_manager: &mut self.file_system_manager,
            memory: &mut self.memory,
        };
        (self.dos.as_mut(), dos_context)
    }

    pub fn get_dos_and_cpu_and_memory(&mut self) -> (&mut Dos, &mut Cpu, &mut Memory) {
        (self.dos.as_mut(), &mut self.cpu, &mut self.memory)
    }

    pub fn get_vga_pal_888(&self) -> Pal888 {
        self.devices.vga().get_pal_888()
    }

    pub fn save_palette(&self) {
        let pal = self.get_vga_pal_888();
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let filename = format!("PAL-{}.BIN", timestamp);

        let result = (|| -> std::io::Result<()> {
            let file = File::create(&filename)?;
            let mut writer = BufWriter::new(file);
            for i in 0..=255 {
                let color = pal[i];
                writer.write_all(&[color.r, color.g, color.b])?;
            }

            let ppm_filename = format!("PAL-{}.ppm", timestamp);
            pal.write_ppm(&ppm_filename, 8)?;
            Ok(())
        })();

        match result {
            Ok(()) => println!("Palette saved to {} and PAL-{}.ppm", filename, timestamp),
            Err(e) => println!("Failed to save palette: {}", e),
        }
    }

    pub fn run_until_next_event(&mut self) -> Attoseconds {
        // Find the device with the soonest event.

        // Time in microseconds
        let mut min_time = f64::INFINITY;

        for device in self.devices.iter() {
            let cycles = device.next_cycles();
            let freq = device.frequency();
            assert!(freq > 0.0);
            let time = cycles as f64 / freq;

            if time < min_time {
                min_time = time;
            }
        }

        // Simulate at most 1ms (1000 microseconds) at a time (5000 cycles @ 5MHz)
        let mut time = min_time.min(1000.0);

        // Run the cpu until the next device event
        if time > 0.0 {
            let cycles = ((time * self.cpu.frequency()) as u64).max(1);
            let (cpu, mut ctx) = self.get_cpu_and_context();
            let (cycles_executed, callback) = cpu.run_cycles(&mut ctx, cycles);

            if let Some((callback, address)) = callback {
                callback(self, address);
            }

            // If the cpu returned early, adjust device simulation time
            if cycles_executed < cycles {
                time = f64::ceil(cycles_executed as f64 / self.cpu.frequency());
            }
        }

        for device in &mut self.devices {
            let cycles = (time * device.frequency()) as u64;

            let mut ctx = DeviceMachineContext {
                cpu: &mut self.cpu,
                memory: &mut self.memory,
            };

            device.run(&mut ctx, cycles);
        }

        Attoseconds::from_microseconds(time)
    }

    pub fn run(&mut self) -> Attoseconds {
        if let Some(input_event_receiver) = self.input_event_receiver.as_ref() {
            for event in input_event_receiver.try_iter() {
                match event {
                    InputEvent::KeyDown(key) => {
                        self.devices.keyboard().key_down(key);
                    }
                    InputEvent::KeyUp(key) => {
                        self.devices.keyboard().key_up(key);
                    }
                    InputEvent::MousePos((x, y)) => {
                        self.dos.mouse.x = ((640.0 * x) as i32).clamp(0, 639) as u16;
                        self.dos.mouse.y = ((200.0 * y) as i32).clamp(0, 199) as u16;
                    }
                    InputEvent::MouseButtons(buttons) => {
                        self.dos.mouse.buttons = (self.dos.mouse.buttons & !0b11)
                            | (buttons.primary_down as u16)
                            | ((buttons.secondary_down as u16) << 1);
                    }
                }
            }
        }

        let time = self.run_until_next_event();
        let vga = self.devices.vga_mut();
        if vga.is_frame_ready() {
            vga.complete_frame(&self.memory);

            // let pal = vga.get_pal_888();
            // self.cpu.pal = pal;

            // vga.write_ppm(&self.memory, addr(0xa000, 0x0000), 320, 200);
        }
        time
    }
}
