use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::console;

const PI: f64 = 3.1415683859;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    console::log_1(&"Hello from Rust!".into());

    let document = web_sys::window().unwrap().document().unwrap();
    let canvas = document.get_element_by_id("smile-canvas").unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas
        // .dyn_into::<web_sys::HtmlCanvasElement>()
        .dyn_into()
        .map_err(|_| ())
        .unwrap();

    let context: web_sys::CanvasRenderingContext2d = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into()
        .unwrap();

    context.begin_path();

    context.arc(75.0, 75.0, 50.0, 0.0, PI * 2.0).unwrap();

    context.stroke();

    Ok(())
}
