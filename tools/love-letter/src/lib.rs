use seed::{prelude::*, *};
use std::collections::BTreeMap;

const FULL_DECK: usize = 16;

/// The available cards to play.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    fn full_deck() -> BTreeMap<Card, usize> {
        let mut deck = BTreeMap::new();
        deck.insert(Card::Guard, 5);
        deck.insert(Card::Priest, 2);
        deck.insert(Card::Baron, 2);
        deck.insert(Card::Handmaid, 2);
        deck.insert(Card::Prince, 2);
        deck.insert(Card::King, 1);
        deck.insert(Card::Countess, 1);
        deck.insert(Card::Princess, 1);
        deck
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

    /// A stylised `<img>` element for this `Card`.
    fn img(&self) -> Node<Msg> {
        self.img_with("card-image")
    }

    /// Like [`img`], but you can customise the CSS class.
    fn img_with(&self, class: &str) -> Node<Msg> {
        img![C![class], attrs! {At::Src => self.image()}]
    }
}

struct Opponent {
    /// Cards that this opponent has played.
    played: Vec<Card>,
    /// Cards this opponent could be holding.
    possible_cards: BTreeMap<Card, usize>,
}

impl Opponent {
    fn new() -> Opponent {
        Opponent {
            played: Vec::new(),
            possible_cards: Card::full_deck(),
        }
    }

    /// Analyses the cards that this player could have, and yields a map of
    /// percentages.
    fn card_probs(&self) -> BTreeMap<Card, usize> {
        let remaining: usize = self.possible_cards.values().sum();

        self.possible_cards
            .iter()
            .map(|(c, n)| (*c, 100 * n / remaining))
            .collect()
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
    /// Cards that have been **played** by the players.
    seen: Vec<Card>,
    /// The possible cards remaining.
    deck: BTreeMap<Card, usize>,
    /// Raw count of the number of cards left in the draw deck.
    deck_size: usize,
    /// The user of the tracker.
    user: User,
    /// The other players.
    opponents: BTreeMap<usize, Opponent>,
}

impl Model {
    fn new() -> Model {
        // TODO Generalize to a custom number of opponents.
        let mut opponents = BTreeMap::new();
        opponents.insert(0, Opponent::new());
        opponents.insert(1, Opponent::new());
        opponents.insert(2, Opponent::new());

        Model {
            tracker: Vec::new(),
            seen: Vec::new(),
            deck: Card::full_deck(),
            deck_size: FULL_DECK,
            opponents,
            user: User::default(),
        }
    }

    // TODO Make this less fragile.
    fn reset(&mut self) {
        let new = Model::new();

        self.tracker = new.tracker;
        self.seen = new.seen;
        self.deck = new.deck;
        self.deck_size = new.deck_size;
        self.opponents = new.opponents;
        self.user = new.user;
    }

    /// A new concrete card has been seen, so add it to the master list of seen
    /// cards, and update each `Opponent`'s possibility list.
    fn seen(&mut self, card: Card) {
        self.seen.push(card);
        match self.deck.get_mut(&card) {
            Some(1) => {
                self.deck.remove(&card);
            }
            Some(n) => {
                *n -= 1;
            }
            None => {}
        }

        for o in self.opponents.values_mut() {
            match o.possible_cards.get_mut(&card) {
                Some(1) => {
                    o.possible_cards.remove(&card);
                }
                Some(n) => {
                    *n -= 1;
                }
                None => {}
            }
        }
    }
}

enum Msg {
    /// Set the tracker state to its initial... state.
    Reset,
    /// A card was played.
    Played(Card),
    /// Note that a player died. They should be removed from the tracker.
    Kill(usize),
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
        Msg::Played(card) => {
            log!(format!("The {:?} card was played.", card));
            model.seen(card);
        }
        Msg::Kill(oid) => {
            log!(format!("Killing player {}.", oid));
            model.opponents.remove(&oid);
        }
    }
}

fn view(model: &Model) -> Vec<Node<Msg>> {
    nodes![
        div!["Love Letter"],
        div![button!["Reset", ev(Ev::Click, |_| Msg::Reset)]],
        view_card_choice(model),
        view_seen_cards(model),
        view_player_grid(model),
    ]
}

fn view_seen_cards(model: &Model) -> Node<Msg> {
    div![
        C!["card-line"],
        b!["Played Cards"],
        model.seen.iter().map(|c| c.img()).collect::<Vec<_>>()
    ]
}

fn view_card_choice(model: &Model) -> Node<Msg> {
    div![
        C!["card-line"],
        model
            .deck
            .keys()
            .map(|c| {
                let card = c.clone();
                button![c.img(), ev(Ev::Click, move |_| Msg::Played(card))]
            })
            .collect::<Vec<_>>()
    ]
}

fn view_player_grid(model: &Model) -> Node<Msg> {
    table![
        tr![th!["Player"], th!["Cards Played"], th!["Hand"],],
        view_user(&model.user),
        model
            .opponents
            .iter()
            .map(|(id, o)| view_opponent(*id, o))
            .collect::<Vec<_>>(),
    ]
}

/// Render a `User`.
fn view_user(user: &User) -> Node<Msg> {
    tr![
        td!["You!"],
        td![div![
            C!["card-line"],
            user.played.iter().map(|c| c.img()).collect::<Vec<_>>()
        ]],
        match &user.hand {
            None => td!["Empty hand..."],
            Some(h) => td![div![
                C!["card-line"],
                h.first.img_with("card-image-user"),
                h.second.as_ref().map(|c| c.img_with("card-image-user"))
            ]],
        }
    ]
}

/// Render an `Opponent`.
fn view_opponent(oid: usize, opponent: &Opponent) -> Node<Msg> {
    let probs = opponent.card_probs();

    tr![
        td![
            format!("Opponent #{}", oid),
            button!["Kill", ev(Ev::Click, move |_| Msg::Kill(oid))]
        ],
        td![div![
            C!["card-line"],
            opponent.played.iter().map(|c| c.img()).collect::<Vec<_>>()
        ]],
        td![div![
            C!["card-line"],
            probs
                .into_iter()
                .map(|(card, prob)| figure![card.img(), figcaption![prob, "%"]])
                .collect::<Vec<_>>()
        ]]
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
