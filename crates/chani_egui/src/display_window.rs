use eframe::egui::{self, Vec2, Vec2b};
use std::sync::{Arc, Mutex};

pub struct DisplayWindow {
    texture: Arc<Mutex<egui::TextureHandle>>,
}

impl DisplayWindow {
    pub fn new(texture: Arc<Mutex<egui::TextureHandle>>) -> Self {
        Self { texture }
    }

    pub fn show(&self, ctx: &egui::Context) {
        egui::Window::new("Display")
            .resizable(true)
            .default_size([640.0, 480.0])
            .scroll(Vec2b::TRUE)
            .show(ctx, |ui| {
                let texture_guard = self.texture.lock().unwrap();
                let texture = &*texture_guard;

                let available_size = ui.available_size();
                // dbg!(available_size);
                let texture_size = texture.size_vec2();
                // dbg!(texture_size);
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

                ui.image((texture.id(), display_size));
            });
    }
}
