use seed::{prelude::*, *};
use std::collections::BTreeMap;

const FULL_DECK: usize = 16;

/// The available cards to play.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

    /// Get the image path for a given card.
    fn image(&self) -> &'static str {
        match self {
            Card::Guard => "guard.jpg",
            Card::Priest => "priest.jpg",
            Card::Baron => "baron.jpg",
            Card::Handmaid => "handmaid.jpg",
            Card::Prince => "prince.jpg",
            Card::King => "king.jpg",
            Card::Countess => "countess.jpg",
            Card::Princess => "princess.jpg",
        }
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

    /// Analyses the cards that this player could have, and yields a map of
    /// percentages.
    fn card_probs(&self) -> BTreeMap<Card, usize> {
        let mut map = BTreeMap::new();

        for card in self.possible_cards.iter() {
            let count = map.entry(card.clone()).or_insert(0);
            *count += 1;
        }

        let remaining = self.possible_cards.len();
        for count in map.values_mut() {
            *count = 100 * (*count) / remaining;
        }

        map
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
    nodes![
        div!["Love Letter"],
        div![button!["Reset", ev(Ev::Click, |_| Msg::Reset)]],
        view_player_grid(model),
    ]
}

fn view_player_grid(model: &Model) -> Node<Msg> {
    table![
        tr![th!["Player"], th!["Cards Played"], th!["Hand"],],
        view_user(&model.user),
        model
            .opponents
            .iter()
            .map(view_opponent)
            .collect::<Vec<_>>(),
    ]
}

/// Render a `User`.
fn view_user(user: &User) -> Node<Msg> {
    tr![
        td!["You!"],
        td![user
            .played
            .iter()
            .map(|c| format!("{:?}", c))
            .collect::<Vec<_>>()],
        match &user.hand {
            None => td!["Empty hand..."],
            Some(h) => td![div![
                C!["card-line"],
                img![C!["card-image"], attrs! {At::Src => h.first.image()}],
                h.second
                    .as_ref()
                    .map(|c| img![C!["card-image"], attrs! {At::Src => c.image()}])
            ]],
        }
    ]
}

/// Render an `Opponent`.
fn view_opponent(opponent: &Opponent) -> Node<Msg> {
    let probs = opponent.card_probs();

    tr![
        td!["Opponent", opponent.alive.then(|| " (Alive)")],
        td![opponent
            .played
            .iter()
            .map(|c| format!("{:?}", c))
            .collect::<Vec<_>>()],
        td![div![
            C!["card-line"],
            probs
                .into_iter()
                .map(|(card, prob)| figure![
                    img![C!["card-image"], attrs! {At::Src => card.image()}],
                    figcaption![prob]
                ])
                .collect::<Vec<_>>()
        ]]
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
