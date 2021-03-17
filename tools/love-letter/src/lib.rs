use seed::{prelude::*, *};
use std::collections::BTreeMap;

const FULL_DECK: usize = 16;

/// The available cards to play.
enum Card {
    Guard,
    Priest,
    Baron,
    Handmaid,
    Prince,
    King,
    Countess,
    Princess,
}

impl Card {
    /// The full deck at the beginning of the game.
    fn full_deck() -> Vec<Card> {
        vec![
            Card::Guard,
            Card::Guard,
            Card::Guard,
            Card::Guard,
            Card::Guard,
            Card::Priest,
            Card::Priest,
            Card::Baron,
            Card::Baron,
            Card::Handmaid,
            Card::Handmaid,
            Card::Prince,
            Card::Prince,
            Card::King,
            Card::Countess,
            Card::Princess,
        ]
    }
}

struct Opponent {
    /// Is this opponent still playing, or have they been knocked out?
    alive: bool,
    /// Cards that this opponent has played.
    played: Vec<Card>,
    /// Cards this opponent could be holding.
    possible_cards: Vec<Card>,
}

impl Opponent {
    fn new() -> Opponent {
        Opponent {
            alive: true,
            played: Vec::new(),
            possible_cards: Card::full_deck(),
        }
    }
}

/// If you have a hand, you have at least one card.
struct Hand {
    first: Card,
    second: Option<Card>,
}

/// The user of the tracker.
#[derive(Default)]
struct User {
    /// The `Hand` of the person using the tracker.
    hand: Option<Hand>,
    /// Cards that the user has played.
    played: Vec<Card>,
}

/// The global state of the tracker.
struct Model {
    /// Cards that haven't been seen.
    tracker: Vec<Card>,
    /// Raw count of the number of cards left in the draw deck.
    deck_size: usize,
    /// The user of the tracker.
    user: User,
    /// The other players.
    opponents: Vec<Opponent>,
}

impl Model {
    fn new() -> Model {
        Model {
            tracker: Vec::new(),
            deck_size: FULL_DECK,
            // TODO Generalize to a custom number of opponents.
            opponents: vec![Opponent::new(), Opponent::new(), Opponent::new()],
            user: User::default(),
        }
    }

    fn reset(&mut self) {
        let new = Model::new();

        self.tracker = new.tracker;
        self.deck_size = new.deck_size;
        self.opponents = new.opponents;
        self.user = new.user;
    }
}

#[derive(Copy, Clone)]
enum Msg {
    /// Set the tracker state to its initial... state.
    Reset,
}

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model::new()
}

fn update(msg: Msg, model: &mut Model, _: &mut impl Orders<Msg>) {
    match msg {
        Msg::Reset => {
            log!("Resetting the Tracker state.");
            model.reset()
        }
    }
}

fn view(model: &Model) -> Vec<Node<Msg>> {
    vec![
        div!["Love Letter"],
        div![button!["Reset", ev(Ev::Click, |_| Msg::Reset)]],
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
