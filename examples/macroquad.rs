use macroquad::prelude::*;

fn window_conf() -> Conf {
    Conf {
        window_title: "puffin_egui with macroquad".to_owned(),
        window_width: 1200,
        window_height: 800,
        high_dpi: true,
        ..Default::default()
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    puffin::set_scopes_on(true); // Remember to call this, or puffin will be disabled!

    while !should_quit() {
        puffin::profile_scope!("main_loop");

        puffin::GlobalProfiler::lock().new_frame(); // call once per frame!

        clear_background(BLACK);

        // Process keys, mouse etc here.

        egui_macroquad::ui(|egui_ctx| {
            puffin_egui::profiler_window(egui_ctx);
        });

        // Draw things behind egui here

        {
            puffin::profile_scope!("egui_macroquad::draw");
            egui_macroquad::draw();
        }

        // Draw things on top of egui here

        sleep_ms(14); // Give us something to inspect

        next_frame().await;
    }
}

fn should_quit() -> bool {
    if cfg!(target_os = "macos") {
        (is_key_down(KeyCode::LeftSuper) || is_key_down(KeyCode::RightSuper))
            && is_key_down(KeyCode::Q)
    } else {
        (is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt))
            && is_key_down(KeyCode::F4)
    }
}

fn sleep_ms(ms: usize) {
    puffin::profile_function!();
    match ms {
        0 => {}
        1 => std::thread::sleep(std::time::Duration::from_millis(1)),
        _ => {
            sleep_ms(ms / 2);
            sleep_ms(ms - (ms / 2));
        }
    }
}
