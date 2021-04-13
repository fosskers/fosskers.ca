use std::ops::Not;
use std::sync::Arc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{Event, HtmlButtonElement, HtmlIFrameElement, HtmlInputElement, Window};

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

/// Get a handle to an element via its ID.
fn element<T: JsCast>(window: &Window, id: &str) -> T {
    window
        .document()
        .unwrap()
        .get_element_by_id(id)
        .unwrap()
        .dyn_into()
        .unwrap()
}

fn click_event(button: &HtmlButtonElement, player: Arc<HtmlIFrameElement>, channel: String) {
    let closure = Closure::wrap(Box::new(move |_: Event| {
        set_iframe_src(&player, &channel);
    }) as Box<dyn FnMut(_)>);
    button
        .add_event_listener_with_callback("click", closure.as_ref().unchecked_ref())
        .unwrap();
    closure.forget();
}

fn set_iframe_src(player: &HtmlIFrameElement, channel: &str) {
    let trimmed = channel.trim();

    if trimmed.is_empty().not() {
        let src = format!(
            "https://player.twitch.tv/?channel={}&parent=localhost&autoplay=false",
            trimmed
        );
        player.set_src(&src);
    }
}

#[wasm_bindgen(start)]
pub fn main() {
    let window = web_sys::window().unwrap();
    let input: HtmlInputElement = element(&window, "twitch-input");
    let watch_btn: HtmlButtonElement = element(&window, "twitch-watch");
    let dan_btn: HtmlButtonElement = element(&window, "twitch-dan");
    let john_btn: HtmlButtonElement = element(&window, "twitch-john");
    let mia_btn: HtmlButtonElement = element(&window, "twitch-mia");

    let player: HtmlIFrameElement = element(&window, "twitch-embedded-player");
    let player_0 = Arc::new(player);
    let player_1 = player_0.clone();
    let player_2 = player_0.clone();

    // Program the Watch button.
    let watch_closure = Closure::wrap(Box::new(move |_: Event| {
        set_iframe_src(&player_0, &input.value());
    }) as Box<dyn FnMut(_)>);
    watch_btn
        .add_event_listener_with_callback("click", watch_closure.as_ref().unchecked_ref())
        .unwrap();
    watch_closure.forget();

    // Program Dan's button.
    let dan_closure = Closure::wrap(Box::new(move |_: Event| {
        set_iframe_src(&player_1, "choccy_soup");
    }) as Box<dyn FnMut(_)>);
    dan_btn
        .add_event_listener_with_callback("click", dan_closure.as_ref().unchecked_ref())
        .unwrap();
    dan_closure.forget();

    // Program John's button.
    let john_closure = Closure::wrap(Box::new(move |_: Event| {
        set_iframe_src(&player_2, "ace_deuce");
    }) as Box<dyn FnMut(_)>);
    john_btn
        .add_event_listener_with_callback("click", john_closure.as_ref().unchecked_ref())
        .unwrap();
    john_closure.forget();
}
