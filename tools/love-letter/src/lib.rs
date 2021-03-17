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

/// The global state of the tracker.
struct Model {
    /// Cards that haven't been seen.
    tracker: Vec<Card>,
    /// Raw count of the number of cards left in the draw deck.
    deck_size: usize,
    /// The other players.
    opponents: Vec<Opponent>,
    /// The `Hand` of the person using the tracker.
    user_hand: Option<Hand>,
    /// Cards that the user has played.
    user_played: Vec<Card>,
}

impl Model {
    fn new() -> Model {
        Model {
            tracker: Vec::new(),
            deck_size: FULL_DECK,
            // TODO Generalize to a custom number of opponents.
            opponents: vec![Opponent::new(), Opponent::new(), Opponent::new()],
            user_hand: None,
            user_played: Vec::new(),
        }
    }
}

#[derive(Copy, Clone)]
enum Msg {
    Increment,
}

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model::new()
}

fn update(msg: Msg, model: &mut Model, _: &mut impl Orders<Msg>) {}

fn view(model: &Model) -> Node<Msg> {
    div!["Love Letter"]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
