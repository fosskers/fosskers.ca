use itertools::Itertools;
use seed::{prelude::*, *};
use std::collections::{BTreeMap, HashMap, HashSet};

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

struct Opponent {
    /// The name of this Opponent.
    name: String,
    /// What card are they known to have?
    has: Option<Card>,
    /// What cards are they known not to have?
    nots: HashSet<Card>,
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
        // TODO Use `filter_drain` once its stable.
        // self.possible_cards = self
        //     .possible_cards
        //     .iter()
        //     .filter(|(c, _)| c > &&card)
        //     .map(|(c, n)| (*c, *n))
        //     .collect();
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
    fn probs(&self, o: &Opponent) -> BTreeMap<Card, usize> {
        match o.has {
            Some(card) => {
                let mut map: BTreeMap<_, _> = ALL_CARDS.iter().map(|c| (*c, 0)).collect();
                let c = map.entry(card).or_insert(100);
                *c = 100;
                map
            }
            None => {
                let rem: usize = self.deck.values().sum();
                let certains: HashMap<Card, usize> = self
                    .opponents
                    .values()
                    .filter_map(|o| o.has)
                    .map(|c| (c, 1))
                    .into_grouping_map()
                    .sum();

                // BELIEVE THE DECK.
                ALL_CARDS
                    .iter()
                    .map(|c| match self.deck.get(c) {
                        _ if o.nots.contains(c) => (*c, 0),
                        None => (*c, 0),
                        Some(0) => (*c, 0),
                        Some(n) => {
                            let m = certains.get(c).unwrap_or(&0);
                            (*c, 100 * (n - m) / rem)
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
    /// A card was played.
    Played(Card),
    /// Mark a seen card as unseen, perhaps if a misclick was made.
    Unplay(usize, Card),
    /// A player is known to have a particular card.
    Has(usize, Card),
    /// A Baron was played.
    Baron(usize, Card),
    /// Note that a player died. They should be removed from the tracker.
    Kill(usize),
}

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model::new(&DUMMY_NAMES)
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
        Msg::Unplay(cid, card) => {
            log!(format!("Unseeing {:?}.", card));
            model.unsee(cid, card);
        }
        Msg::Has(oid, card) => {
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
            log!(format!("Killing player {}.", oid));
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
                        ev(Ev::Click, move |_| Msg::Played(card))
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
                        ev(Ev::Click, move |_| Msg::Unplay(id, card))
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
                        C![(prob == 0).then(|| "zero")],
                        attrs! {
                            At::Type => "image",
                            At::Src => card.image()
                        },
                        ev(Ev::Click, move |_| Msg::Has(oid, card))
                    ],
                    figcaption![prob, "%"],
                    button!["Baron'd", ev(Ev::Click, move |_| Msg::Baron(oid, card))]
                ])
                .collect::<Vec<_>>()
        ]]
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
