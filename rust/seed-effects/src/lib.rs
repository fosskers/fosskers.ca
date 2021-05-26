use seed::{prelude::*, *};
use serde::Deserialize;

const URL: &str = "https://jsonplaceholder.typicode.com/posts/1";

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model {
        title: None,
        working: None,
    }
}

#[derive(Deserialize)]
struct Resp {
    title: String,
}

enum Working {
    Now,
    Success,
    Failed,
}

struct Model {
    title: Option<String>,
    working: Option<Working>,
}

enum Msg {
    SendReq,
    Fetched(fetch::Result<Resp>),
}

fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::SendReq => {
            orders
                // .skip()
                .perform_cmd(async { Msg::Fetched(send_message().await) });
            model.working.replace(Working::Now);
        }
        Msg::Fetched(Ok(r)) => {
            model.title.replace(r.title);
            model.working.replace(Working::Success);
        }
        Msg::Fetched(Err(e)) => {
            log!(e);
            model.working.replace(Working::Failed);
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
    div![C!["web-effects"], v_json(model)]
}

fn v_json(model: &Model) -> Node<Msg> {
    div![
        C!["field", "is-grouped"],
        p![
            C!["control"],
            button![
                C!["button", "is-info"],
                "Fetch JSON",
                ev(Ev::Click, |_| Msg::SendReq)
            ]
        ],
        p![
            C!["control"],
            input![
                C!["input"],
                attrs!(At::Type => "text", At::Disabled => ""),
                model.title.as_deref().map(|s| attrs!(At::Value => s))
            ]
        ],
        match model.working {
            None => None,
            Some(Working::Now) => {
                Some(p![
                    C!["control"],
                    span![C!["icon"], i![C!["fas", "fa-question-circle"]]]
                ])
            }
            Some(Working::Success) => {
                Some(p![
                    C!["control"],
                    span![C!["icon"], i![C!["fas", "fa-check"]]]
                ])
            }
            Some(Working::Failed) => {
                Some(p![
                    C!["control"],
                    span![C!["icon"], i![C!["fas", "fa-times-circle"]]]
                ])
            }
        }
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
