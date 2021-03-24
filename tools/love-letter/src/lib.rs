use itertools::Itertools;
use seed::{prelude::*, *};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Not as OtherNot;

const FULL_DECK: usize = 16;

const ALL_CARDS: [Card; 8] = [
    Card::Guard,
    Card::Priest,
    Card::Baron,
    Card::Handmaid,
    Card::Prince,
    Card::King,
    Card::Countess,
    Card::Princess,
];

/// Placeholder opponent names.
const DUMMY_NAMES: [&str; 3] = ["Sam", "Pippin", "Merry"];

/// The available cards to play.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            Card::Guard => "/assets/guard.jpg",
            Card::Priest => "/assets/priest.jpg",
            Card::Baron => "/assets/baron.jpg",
            Card::Handmaid => "/assets/handmaid.jpg",
            Card::Prince => "/assets/prince.jpg",
            Card::King => "/assets/king.jpg",
            Card::Countess => "/assets/countess.jpg",
            Card::Princess => "/assets/princess.jpg",
        }
    }
}

/// Cards we know the `Opponent` doesn't have, differentiated by the source of
/// that knowledge.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Not {
    Guard(Card),
    Baron(Card),
    PrKing(Card),
}

impl Not {
    /// The raw `Card` we know is contained in this `Not`.
    fn card(&self) -> Card {
        match self {
            Not::Guard(c) | Not::Baron(c) | Not::PrKing(c) => *c,
        }
    }

    /// Is this a `Not::Guard`?
    fn is_guard(&self) -> bool {
        match self {
            Not::Guard(_) => true,
            _ => false,
        }
    }
}

struct Opponent {
    /// The name of this Opponent.
    name: String,
    /// What card are they known to have?
    has: Option<Card>,
    /// What cards are they known not to have?
    nots: HashSet<Not>,
}

impl Opponent {
    fn new(name: String) -> Opponent {
        Opponent {
            name,
            has: None,
            nots: HashSet::new(),
        }
    }

    /// Mark that the `Opponent` has a specific card.
    fn only_has(&mut self, card: Card) {
        self.has.replace(card);
        self.nots.clear();
    }

    /// This `Opponent` survived a Baron, revealing the given card.
    fn baron(&mut self, card: Card) {
        ALL_CARDS.iter().filter(|c| c <= &&card).for_each(|c| {
            self.nots.insert(Not::Baron(*c));
        })
    }

    /// An `Opponent` played a specific card and special circumstances must be
    /// considered when updating the knowledge we have on them.
    fn played(&mut self, card: Card) {
        match card {
            Card::Prince | Card::King => {
                self.nots.insert(Not::PrKing(Card::Countess));
            }
            _ => {}
        }

        if self.nots.contains(&Not::Guard(card)).not() {
            // TODO Use `filter_drain` once stable.
            self.nots = self
                .nots
                .iter()
                .filter(|n| n.is_guard().not())
                .map(|n| *n)
                .collect();
        }

        self.nots.remove(&Not::Baron(card));
    }

    /// Do we know that this `Opponent` *doesn't* have the given card?
    fn has_not(&self, card: &Card) -> bool {
        self.nots.iter().any(|not| match not {
            Not::Guard(c) => c == card,
            Not::Baron(c) => c == card,
            Not::PrKing(c) => c == card,
        })
    }
}

/// The global state of the tracker.
struct Model {
    // /// Opponent names.
    // names: Vec<String>,
    /// Cards that haven't been seen.
    tracker: Vec<Card>,
    /// Cards that have been **played** by the players.
    seen: BTreeMap<usize, Card>,
    /// The possible cards remaining.
    deck: BTreeMap<Card, usize>,
    /// Raw count of the number of cards left in the draw deck.
    deck_size: usize,
    /// The other players.
    opponents: BTreeMap<usize, Opponent>,
}

impl Model {
    fn new(names: &[&str]) -> Model {
        // TODO Generalize to a custom number of opponents.
        let mut opponents = BTreeMap::new();
        for (i, n) in names.iter().enumerate() {
            opponents.insert(i, Opponent::new(n.to_string()));
        }

        Model {
            tracker: Vec::new(),
            seen: BTreeMap::new(),
            deck: Card::full_deck(),
            deck_size: FULL_DECK,
            opponents,
        }
    }

    // TODO Make this less fragile.
    fn reset(&mut self) {
        let new = Model::new(&DUMMY_NAMES);

        self.tracker = new.tracker;
        self.seen = new.seen;
        self.deck = new.deck;
        self.deck_size = new.deck_size;
        self.opponents = new.opponents;
    }

    /// A new concrete card has been seen, so add it to the master list of seen
    /// cards.
    fn seen(&mut self, card: Card) {
        let id = self.seen.keys().max().map(|max| *max).unwrap_or(0);
        self.seen.insert(id + 1, card);
        match self.deck.get_mut(&card) {
            Some(1) => {
                self.deck.remove(&card);
            }
            Some(n) => {
                *n -= 1;
            }
            _ => {}
        }
    }

    /// Unsee a card that was perhaps added in error from a misclick.
    fn unsee(&mut self, cid: usize, card: Card) {
        self.seen.remove(&cid);

        let entry = self.deck.entry(card).or_insert(0);
        *entry += 1;
    }

    /// For a particular `Opponent`, what are the probabilities that they have
    /// each card?
    fn probs(&self, o: &Opponent) -> BTreeMap<Card, f32> {
        match o.has {
            Some(card) => {
                let mut map: BTreeMap<_, _> = ALL_CARDS.iter().map(|c| (*c, 0.0)).collect();
                let c = map.entry(card).or_insert(100.0);
                *c = 100.0;
                map
            }
            None => {
                let certains: HashMap<Card, usize> = self
                    .opponents
                    .values()
                    .filter_map(|o| o.has)
                    .map(|c| (c, 1))
                    .into_grouping_map()
                    .sum();
                let notted: usize = o.nots.iter().filter_map(|c| self.deck.get(&c.card())).sum();
                let rem = (self.deck.values().sum::<usize>()
                    - certains.values().sum::<usize>()
                    - notted) as f32;

                // BELIEVE THE DECK.
                ALL_CARDS
                    .iter()
                    .map(|c| match self.deck.get(c) {
                        _ if o.has_not(c) => (*c, 0.0),
                        // Avoids div-by-zero if the deck is empty.
                        None | Some(0) => (*c, 0.0),
                        Some(n) => {
                            let m = certains.get(c).unwrap_or(&0);
                            (*c, 100.0 * ((n - m) as f32) / rem)
                        }
                    })
                    .collect()
            }
        }
    }
}

enum Msg {
    /// Set the tracker state to its initial... state.
    Reset,
    /// Forget special knowledge for a particular player.
    ForgetPlayer(usize),
    /// A card was seen.
    Seen(Card),
    /// Mark a seen card as unseen, perhaps if a misclick was made.
    Unsee(usize, Card),
    /// A card was played by a specific player.
    Played(usize, Card),
    /// A "Guard miss" occurred.
    Guard(usize, Card),
    /// A Priest was played.
    Priest(usize, Card),
    /// A Baron was played.
    Baron(usize, Card),
    /// Note that a player died. They should be removed from the tracker.
    Kill(usize),
}

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model::new(&DUMMY_NAMES)
}

fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::Reset => model.reset(),
        Msg::Seen(card) => model.seen(card),
        Msg::Unsee(cid, card) => model.unsee(cid, card),
        Msg::Played(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.played(card);
            }
            update(Msg::Seen(card), model, orders);
        }
        Msg::Guard(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                log!("Guard miss!");
                o.nots.insert(Not::Guard(card));
            }
        }
        Msg::Priest(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.only_has(card);
            }
        }
        Msg::Baron(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                log!(format!("Opp {} survived a Baron", oid,));
                o.baron(card);
            }
        }
        Msg::Kill(oid) => {
            model.opponents.remove(&oid);
        }
        Msg::ForgetPlayer(oid) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                log!(format!("Forgetting knowledge about player {}.", oid));
                o.has.take();
                o.nots.clear();
            }
        }
    }
}

fn view(model: &Model) -> Vec<Node<Msg>> {
    nodes![
        div!["Love Letter"],
        div![button!["Reset Game", ev(Ev::Click, |_| Msg::Reset)]],
        hr![],
        view_card_choice(model),
        hr![],
        view_seen_cards(model),
        hr![],
        view_player_grid(model),
    ]
}

fn view_card_choice(model: &Model) -> Vec<Node<Msg>> {
    vec![
        div![b!["Remaining Unseen Cards"]],
        div![
            C!["card-line"],
            model
                .deck
                .keys()
                .map(|c| {
                    let card = c.clone();
                    div![input![
                        attrs! {At::Type => "image", At::Src => c.image()},
                        ev(Ev::Click, move |_| Msg::Seen(card))
                    ]]
                })
                .collect::<Vec<_>>()
        ],
    ]
}

fn view_seen_cards(model: &Model) -> Vec<Node<Msg>> {
    vec![
        div![b!["Seen Cards"]],
        div![
            C!["card-line"],
            model
                .seen
                .iter()
                .map(|(cid, c)| {
                    let card = c.clone();
                    let id = cid.clone();
                    div![input![
                        attrs! { At::Type => "image", At::Src => card.image()},
                        ev(Ev::Click, move |_| Msg::Unsee(id, card))
                    ]]
                })
                .collect::<Vec<_>>()
        ],
    ]
}

fn view_player_grid(model: &Model) -> Node<Msg> {
    table![model
        .opponents
        .iter()
        .map(|(id, o)| view_opponent(model, *id, o))
        .collect::<Vec<_>>(),]
}

/// Render an `Opponent`.
fn view_opponent(model: &Model, oid: usize, opponent: &Opponent) -> Node<Msg> {
    let probs = model.probs(opponent);

    tr![
        td![
            div![&opponent.name],
            div![
                button!["Kill", ev(Ev::Click, move |_| Msg::Kill(oid))],
                button!["Reset", ev(Ev::Click, move |_| Msg::ForgetPlayer(oid))]
            ]
        ],
        td![div![
            C!["card-line"],
            probs
                .into_iter()
                .map(|(card, prob)| figure![
                    input![
                        C![(prob == 0.0).then(|| "zero")],
                        attrs! {
                            At::Type => "image",
                            At::Src => card.image()
                        },
                        ev(Ev::Click, move |_| Msg::Played(oid, card))
                    ],
                    figcaption![format!("{:.1}%", prob)],
                    (card != Card::Guard)
                        .then(|| button!["G", ev(Ev::Click, move |_| Msg::Guard(oid, card))]),
                    button!["P", ev(Ev::Click, move |_| Msg::Priest(oid, card))],
                    button!["B", ev(Ev::Click, move |_| Msg::Baron(oid, card))]
                ])
                .collect::<Vec<_>>()
        ]]
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
