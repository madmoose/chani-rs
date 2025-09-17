use std::sync::{Arc, Condvar, Mutex};
use std::thread::{self, JoinHandle};

use chani_core::machine::Machine;

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Stopped,
    Running,
    Paused,
}

pub struct MachineRunner {
    state: Arc<Mutex<State>>,
    condvar: Arc<Condvar>,
    machine: Arc<Mutex<Machine>>,
    thread_handle: Option<JoinHandle<()>>,
}

impl MachineRunner {
    pub fn new(machine: Machine) -> Self {
        Self {
            state: Arc::new(Mutex::new(State::Stopped)),
            condvar: Arc::new(Condvar::new()),
            machine: Arc::new(Mutex::new(machine)),
            thread_handle: None,
        }
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

                let handle = thread::spawn(move || {
                    Self::run_loop(state_clone, condvar_clone, machine_clone);
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

    fn run_loop(state: Arc<Mutex<State>>, condvar: Arc<Condvar>, machine: Arc<Mutex<Machine>>) {
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
                    continue;
                }
                State::Running => {
                    // Release the state lock before running the machine
                    drop(state);

                    // Run the machine
                    if let Ok(mut machine_guard) = machine.lock() {
                        machine_guard.run();
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
