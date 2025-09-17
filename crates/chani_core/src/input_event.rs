use std::sync::mpsc;

use crate::Key;

pub type InputEventSender = mpsc::Sender<InputEvent>;
pub type InputEventReceiver = mpsc::Receiver<InputEvent>;

pub enum InputEvent {
    KeyDown(Key),
    KeyUp(Key),
}
