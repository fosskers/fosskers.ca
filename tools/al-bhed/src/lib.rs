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
    let window = web_sys::window().unwrap();
    let english = english_text(&window);
    let al_bhed = al_bhed_text(&window);
    let en_copy0 = english.clone();
    let en_copy1 = english.clone();
    let al_copy0 = al_bhed.clone();
    let al_copy1 = al_bhed.clone();

    let english_closure = Closure::wrap(Box::new(move |_: Event| {
        let text = en_copy0.value();
        let tran = text
            .chars()
            .map(|c| albhed::to_al_bhed(c).unwrap_or('?'))
            .collect::<String>();
        al_copy0.set_value(&tran);
    }) as Box<dyn FnMut(_)>);
    english.set_oninput(Some(english_closure.as_ref().unchecked_ref()));
    english_closure.forget();

    let al_bhed_closure = Closure::wrap(Box::new(move |_: Event| {
        let text = al_copy1.value();
        let tran = text
            .chars()
            .map(|c| albhed::from_al_bhed(c).unwrap_or('?'))
            .collect::<String>();
        en_copy1.set_value(&tran);
    }) as Box<dyn FnMut(_)>);
    al_bhed.set_oninput(Some(al_bhed_closure.as_ref().unchecked_ref()));
    al_bhed_closure.forget();
}
