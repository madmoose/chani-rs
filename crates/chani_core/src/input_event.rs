use std::sync::mpsc;

use crate::{Key, MouseButtons};

pub type InputEventSender = mpsc::Sender<InputEvent>;
pub type InputEventReceiver = mpsc::Receiver<InputEvent>;

pub enum InputEvent {
    KeyDown(Key),
    KeyUp(Key),
    MousePos((f32, f32)),
    MouseButtons(MouseButtons),
}
