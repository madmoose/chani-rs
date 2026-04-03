use chani_core::{
    MouseButtons, MouseState,
    input_event::{InputEvent, InputEventSender},
};
use eframe::egui::{self, PointerButton, Vec2, Vec2b};
use std::sync::{Arc, Mutex};

pub struct DisplayWindow {
    texture: Arc<Mutex<egui::TextureHandle>>,
    input_tx: InputEventSender,
    mouse_state: Option<MouseState>,
}

impl DisplayWindow {
    pub fn new(texture: Arc<Mutex<egui::TextureHandle>>, input_tx: InputEventSender) -> Self {
        Self {
            texture,
            input_tx,
            mouse_state: None,
        }
    }

    pub fn show(&mut self, ctx: &egui::Context) {
        egui::Window::new("Display")
            .resizable(true)
            .default_size([640.0, 480.0])
            .scroll(Vec2b::TRUE)
            .show(ctx, |ui| {
                let texture_guard = self.texture.lock().unwrap();
                let texture = &*texture_guard;

                let available_size = ui.available_size();
                let texture_size = texture.size_vec2();
                if texture_size.min_elem() < 1.0 {
                    return;
                }

                let scale_factor = available_size / texture_size;
                let scale_factor = scale_factor.floor().min_elem().max(1.0);

                let aspect_ratio = texture_size.y / texture_size.x;
                let aspect_ratio_correction = 6.0 / 5.0;

                let w = scale_factor * texture_size.x;
                let h = w * aspect_ratio * aspect_ratio_correction;

                let display_size = Vec2::new(w, h);

                let response = ui.image((texture.id(), display_size));

                // Get mouse position and button states
                if let Some(cursor_pos) = ui.ctx().pointer_hover_pos() {
                    let image_rect = response.rect;
                    if image_rect.contains(cursor_pos) {
                        let relative_pos = cursor_pos - image_rect.min;
                        let normalized_x = relative_pos.x / image_rect.width();
                        let normalized_y = relative_pos.y / image_rect.height();

                        let mouse_buttons = ui.ctx().input(|i| MouseButtons {
                            primary_down: i.pointer.button_down(PointerButton::Primary),
                            secondary_down: i.pointer.button_down(PointerButton::Secondary),
                            middle_down: i.pointer.button_down(PointerButton::Middle),
                        });

                        let new_mouse_state = MouseState {
                            position: (normalized_x, normalized_y),
                            buttons: mouse_buttons,
                        };

                        let position_changed = self
                            .mouse_state
                            .as_ref()
                            .is_none_or(|st| st.position != new_mouse_state.position);

                        let buttons_changed = self
                            .mouse_state
                            .as_ref()
                            .is_none_or(|st| st.buttons != new_mouse_state.buttons);

                        if position_changed {
                            _ = self
                                .input_tx
                                .send(InputEvent::MousePos(new_mouse_state.position));
                        }

                        if buttons_changed {
                            _ = self
                                .input_tx
                                .send(InputEvent::MouseButtons(new_mouse_state.buttons));
                        }

                        self.mouse_state = Some(new_mouse_state);
                    }
                }
            });
    }
}
