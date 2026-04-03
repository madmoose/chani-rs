use std::sync::{Arc, Condvar, Mutex, mpsc};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use chani_core::machine::Machine;

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Stopped,
    Running,
    Paused,
}

#[derive(Debug, Clone)]
pub enum MachineCommand {
    SavePalette,
}

pub struct MachineRunner {
    state: Arc<Mutex<State>>,
    condvar: Arc<Condvar>,
    machine: Arc<Mutex<Machine>>,
    thread_handle: Option<JoinHandle<()>>,
    command_tx: mpsc::Sender<MachineCommand>,
    command_rx: Arc<Mutex<mpsc::Receiver<MachineCommand>>>,
    // target_instant: Option<Instant>,
}

impl MachineRunner {
    pub fn new(machine: Machine) -> Self {
        let (command_tx, command_rx) = mpsc::channel();
        Self {
            state: Arc::new(Mutex::new(State::Stopped)),
            condvar: Arc::new(Condvar::new()),
            machine: Arc::new(Mutex::new(machine)),
            thread_handle: None,
            command_tx,
            command_rx: Arc::new(Mutex::new(command_rx)),
            // target_instant: None,
        }
    }

    pub fn send_command(&self, command: MachineCommand) {
        let _ = self.command_tx.send(command);
    }

    pub fn start(&mut self) {
        let mut state = self.state.lock().unwrap();
        match *state {
            State::Stopped => {
                *state = State::Running;
                drop(state); // Release the lock before starting the thread

                let state_clone = Arc::clone(&self.state);
                let condvar_clone = Arc::clone(&self.condvar);
                let machine_clone = Arc::clone(&self.machine);
                let command_rx_clone = Arc::clone(&self.command_rx);

                let handle = thread::spawn(move || {
                    Self::run_loop(state_clone, condvar_clone, machine_clone, command_rx_clone);
                });

                self.thread_handle = Some(handle);
            }
            State::Paused => {
                *state = State::Running;
                self.condvar.notify_one();
            }
            State::Running => {
                // Already running, do nothing
            }
        }
    }

    pub fn pause(&self) {
        let mut state = self.state.lock().unwrap();
        if *state == State::Running {
            *state = State::Paused;
        }
    }

    pub fn stop(&mut self) {
        {
            let mut state = self.state.lock().unwrap();
            *state = State::Stopped;
            self.condvar.notify_one();
        }

        // Wait for the thread to finish
        if let Some(handle) = self.thread_handle.take() {
            handle.join().unwrap();
        }
    }

    pub fn get_state(&self) -> State {
        self.state.lock().unwrap().clone()
    }

    fn run_loop(
        state: Arc<Mutex<State>>,
        condvar: Arc<Condvar>,
        machine: Arc<Mutex<Machine>>,
        command_rx: Arc<Mutex<mpsc::Receiver<MachineCommand>>>,
    ) {
        let mut next_target = Instant::now();

        loop {
            let mut state = state.lock().unwrap();

            match *state {
                State::Stopped => {
                    break;
                }
                State::Paused => {
                    // Wait until state changes
                    while matches!(*state, State::Paused) {
                        state = condvar.wait(state).unwrap();
                    }
                    // Reset timing when resuming from pause
                    next_target = Instant::now();
                    continue;
                }
                State::Running => {
                    // Release the state lock before running the machine
                    drop(state);

                    // Process any pending commands
                    if let Ok(command_rx) = command_rx.lock() {
                        for command in command_rx.try_iter() {
                            if let Ok(machine_guard) = machine.lock() {
                                match command {
                                    MachineCommand::SavePalette => {
                                        machine_guard.save_palette();
                                    }
                                }
                            }
                        }
                    }

                    // Run the machine and get simulated time
                    let simulated_time = if let Ok(mut machine_guard) = machine.lock() {
                        machine_guard.run()
                    } else {
                        continue;
                    };
                    // println!("Simulated {}µs", simulated_time.to_microseconds());

                    // Convert attoseconds to Duration
                    let simulated_micros = simulated_time.to_microseconds();
                    let simulated_duration = Duration::from_micros(simulated_micros as u64);

                    // Calculate when we should wake up next
                    next_target += simulated_duration;

                    // Sleep until the target time
                    let now = Instant::now();
                    if next_target > now {
                        // println!("Sleeping {}µs", (next_target - now).as_micros());
                        thread::sleep(next_target - now);
                    } else {
                        // If we're behind, don't sleep but reset target to avoid drift accumulation
                        next_target = now;
                    }
                }
            }
        }
    }
}

impl Drop for MachineRunner {
    fn drop(&mut self) {
        // Ensure the thread is stopped when the MachineRunner is dropped
        if self.thread_handle.is_some() {
            self.stop();
        }
    }
}
