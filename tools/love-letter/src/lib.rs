use seed::{prelude::*, *};

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model::default()
}

type Model = i32;

#[derive(Copy, Clone)]
enum Msg {
    Increment,
}

fn update(msg: Msg, model: &mut Model, _: &mut impl Orders<Msg>) {
    match msg {
        Msg::Increment => *model += 1,
    }
}

fn view(model: &Model) -> Node<Msg> {
    div![
        "This is a counter: ",
        C!["counter"],
        button![model, ev(Ev::Click, |_| Msg::Increment),],
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
