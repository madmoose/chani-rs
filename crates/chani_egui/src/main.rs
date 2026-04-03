#![feature(generic_const_exprs)]
#![allow(incomplete_features)]

// mod bitset;
mod display_window;
mod key;
mod machine_runner;

use std::{
    sync::{Arc, Mutex, mpsc},
    thread,
};

use chani_core::{
    dos::ExecArgs,
    frame::{Frame, FrameReceiver},
    input_event::{InputEvent, InputEventSender},
    machine::Machine,
};
use eframe::egui;

use display_window::DisplayWindow;
use machine_runner::MachineRunner;

use crate::key::key_from_egui_key;

struct App {
    // sidebar_width: f32,
    // timeline_height: f32,
    framebuffer_window: DisplayWindow,
    machine_runner: MachineRunner,
    input_tx: InputEventSender,
}

impl App {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let texture = cc.egui_ctx.load_texture(
            "framebuffer",
            egui::ColorImage::new([1, 1], vec![egui::Color32::BLACK]),
            egui::TextureOptions::NEAREST,
        );

        let current_texture = Arc::new(Mutex::new(texture));
        let (frame_tx, frame_rx) = mpsc::channel::<Frame>();
        let (input_tx, input_rx) = mpsc::channel::<InputEvent>();

        let mut machine = Machine::new();
        machine.set_frame_sender(frame_tx);
        machine.set_input_receiver(input_rx);

        machine
            .file_system_manager
            .set_drive_at_index::<chani_core::file_system::native::NativeFileSystem>(
                2,
                "/Users/madmoose/Games/cryo-dune/pc-3.7-cd",
            )
            .expect("Failed to mount C-drive");

        {
            let (dos, mut ctx) = machine.get_dos_and_context();
            dos.exec_load_and_execute(&mut ctx, br"DNCDPRG.EXE", 0, ExecArgs::Str(""))
                .expect("Failed to load executable");
        }

        let machine_runner = MachineRunner::new(machine);

        Self::start_frame_handler(cc.egui_ctx.clone(), frame_rx, current_texture.clone());
        // Self::start_machine_thread(machine.clone());

        Self {
            // sidebar_width: 200.0,
            // timeline_height: 100.0,
            framebuffer_window: DisplayWindow::new(current_texture, input_tx.clone()),
            machine_runner,
            input_tx,
        }
    }

    fn start_frame_handler(
        ctx: egui::Context,
        frame_receiver: FrameReceiver,
        current_texture: Arc<Mutex<egui::TextureHandle>>,
    ) {
        thread::spawn(move || {
            println!("Starting frame handler thread");
            while let Ok(frame) = frame_receiver.recv() {
                let color_image = egui::ColorImage::from_rgba_unmultiplied(
                    [frame.width(), frame.height()],
                    frame.data(),
                );

                // Update the shared texture
                {
                    let mut texture_guard = current_texture.lock().unwrap();
                    texture_guard.set(color_image, egui::TextureOptions::NEAREST);
                }

                // Request a repaint on the main thread
                ctx.request_repaint();
            }
            println!("Ending frame handler thread");
        });
    }
}

impl eframe::App for App {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        let control_bar_height = 30.0;

        // Top menu bar
        egui::Panel::top("menu_bar").show_inside(ui, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Save palette").clicked() {
                        self.machine_runner
                            .send_command(machine_runner::MachineCommand::SavePalette);
                        ui.close();
                    }
                });
            });
        });

        // Bottom control bar
        egui::Panel::bottom("control_bar")
            .exact_size(control_bar_height)
            .resizable(false)
            .show_inside(ui, |ui| {
                ui.horizontal(|ui| {
                    ui.label("Control Bar:");
                    if ui.button("Play").clicked() {
                        self.machine_runner.start();
                    }
                    if ui.button("Pause").clicked() {
                        self.machine_runner.pause();
                    }
                    if ui.button("Stop").clicked() {
                        self.machine_runner.stop();
                    }
                    ui.separator();

                    let status = match self.machine_runner.get_state() {
                        machine_runner::State::Stopped => "Status: Stopped",
                        machine_runner::State::Running => "Status: Running",
                        machine_runner::State::Paused => "Status: Paused",
                    };

                    ui.label(status);
                });
            });

        self.framebuffer_window.show(ui.ctx());
    }

    fn raw_input_hook(&mut self, _ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        raw_input.events.iter().for_each(|e| {
            if let egui::Event::Key {
                key,
                pressed,
                repeat: false,
                ..
            } = e
            {
                let event = if *pressed {
                    InputEvent::KeyDown(key_from_egui_key(*key))
                } else {
                    InputEvent::KeyUp(key_from_egui_key(*key))
                };
                _ = self.input_tx.send(event);
            }
        });
    }
}

fn main() -> eframe::Result {
    let native_options = eframe::NativeOptions::default();

    eframe::run_native(
        "Chani — Simulflow Emulator",
        native_options,
        Box::new(|cc| Ok(Box::new(App::new(cc)))),
    )
}
