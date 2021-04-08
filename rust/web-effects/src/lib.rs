use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{Event, HtmlButtonElement, Window};

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn print_button(window: &Window) -> HtmlButtonElement {
    window
        .document()
        .unwrap()
        .get_element_by_id("print-button")
        .unwrap()
        .dyn_into()
        .unwrap()
}

#[wasm_bindgen(start)]
pub fn main() {
    let window = web_sys::window().unwrap();

    // Program the Print button.
    let print_button = print_button(&window);
    let print_closure = Closure::wrap(Box::new(move |_: Event| {
        let _ = window.print();
    }) as Box<dyn FnMut(_)>);
    print_button
        .add_event_listener_with_callback("click", print_closure.as_ref().unchecked_ref())
        .unwrap();
    print_closure.forget();
}
