use seed::{prelude::*, *};
use serde::Deserialize;

const URL: &str = "https://jsonplaceholder.typicode.com/posts/1";

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model { title: None }
}

#[derive(Deserialize)]
struct Resp {
    title: String,
}

struct Model {
    title: Option<String>,
}

enum Msg {
    SendReq,
    Fetched(fetch::Result<Resp>),
}

fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::SendReq => {
            orders
                .skip()
                .perform_cmd(async { Msg::Fetched(send_message().await) });
        }
        Msg::Fetched(Ok(r)) => {
            model.title.replace(r.title);
        }
        Msg::Fetched(Err(e)) => {
            log!(e);
        }
    }
}

async fn send_message() -> fetch::Result<Resp> {
    Request::new(URL)
        .method(Method::Get)
        .fetch()
        .await?
        .check_status()?
        .json()
        .await
}

fn view(model: &Model) -> Node<Msg> {
    div![
        button!["Fetch JSON", ev(Ev::Click, |_| Msg::SendReq)],
        model.title.as_deref().map(|s| p![s])
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
