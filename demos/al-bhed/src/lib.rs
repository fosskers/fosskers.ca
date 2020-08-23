use js_sys::Math;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn english_text(window: &Window) -> HtmlTextAreaElement {
    window
        .document()
        .unwrap()
        .get_element_by_id("english-text")
        .unwrap()
        .dyn_into()
        .unwrap()
}

fn al_bhed_text(window: &Window) -> HtmlTextAreaElement {
    window
        .document()
        .unwrap()
        .get_element_by_id("al-bhed-text")
        .unwrap()
        .dyn_into()
        .unwrap()
}

#[wasm_bindgen(start)]
pub fn main() {
    // Configure the canvas.
    let window = web_sys::window().unwrap();

    let english = english_text(&window);
    let al_bhed = al_bhed_text(&window);

    english.set_inner_html("From Rust code!");
    al_bhed.set_inner_html(
        &"From Rust code!"
            .chars()
            .filter_map(albhed::to_al_bhed)
            .collect::<String>(),
    );

    // // Program the `Reset` button.
    // let reset_button = reset_button(&window);
    // let reset_closure = Closure::wrap(Box::new(move |_: Event| {
    //     let mut uni = shared_uni_1.lock().unwrap();
    //     *uni = Universe::new();
    // }) as Box<dyn FnMut(_)>);
    // reset_button
    //     .add_event_listener_with_callback("click", reset_closure.as_ref().unchecked_ref())
    //     .unwrap();
    // reset_closure.forget();

    // // Program the `Pause` button.
    // let shared_handler_0 = Arc::new(Mutex::new(Some(0)));
    // let shared_handler_1 = shared_handler_0.clone();
    // let pause_button = pause_button(&window);
    // let pause_closure = Closure::wrap(Box::new(move |_: Event| {
    //     let mut animation_handler = shared_handler_0.lock().unwrap();
    //     match *animation_handler {
    //         None => {
    //             *animation_handler = Some(0);
    //             play(&window, &context, &shared_uni_2, &shared_handler_0);
    //         }
    //         Some(h) => {
    //             let _ = window.cancel_animation_frame(h);
    //             *animation_handler = None;
    //         }
    //     }
    // }) as Box<dyn FnMut(_)>);
    // pause_button
    //     .add_event_listener_with_callback("click", pause_closure.as_ref().unchecked_ref())
    //     .unwrap();
    // pause_closure.forget();
}
