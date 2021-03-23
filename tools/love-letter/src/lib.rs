use seed::{prelude::*, *};
use std::collections::BTreeMap;
use std::ops::Not;

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
    /// Cards this opponent could be holding.
    possible_cards: BTreeMap<Card, usize>,
}

impl Opponent {
    fn new() -> Opponent {
        Opponent {
            possible_cards: Card::full_deck(),
        }
    }

    /// Analyses the cards that this player could have, and yields a map of
    /// percentages.
    fn card_probs(&self) -> BTreeMap<Card, usize> {
        let rem: usize = self.possible_cards.values().sum();

        ALL_CARDS
            .iter()
            .map(|c| match self.possible_cards.get(c) {
                Some(n) => (*c, 100 * n / rem),
                None => (*c, 0),
            })
            .collect()
    }

    /// Mark that the `Opponent` has a specific card.
    fn only_has(&mut self, card: Card) {
        let mut poss = BTreeMap::new();
        poss.insert(card, 1);
        self.possible_cards = poss;
    }

    /// Do we know what this `Opponent` has?
    fn certain(&self) -> bool {
        self.possible_cards.len() == 1
    }

    /// This `Opponent` survived a Baron, revealing the given card.
    fn baron(&mut self, card: Card) {
        // TODO Use `filter_drain` once its stable.
        self.possible_cards = self
            .possible_cards
            .iter()
            .filter(|(c, _)| c > &&card)
            .map(|(c, n)| (*c, *n))
            .collect();
    }
}

/// The global state of the tracker.
struct Model {
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
    fn new() -> Model {
        // TODO Generalize to a custom number of opponents.
        let mut opponents = BTreeMap::new();
        opponents.insert(0, Opponent::new());
        opponents.insert(1, Opponent::new());
        opponents.insert(2, Opponent::new());

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
        let new = Model::new();

        self.tracker = new.tracker;
        self.seen = new.seen;
        self.deck = new.deck;
        self.deck_size = new.deck_size;
        self.opponents = new.opponents;
    }

    /// A new concrete card has been seen, so add it to the master list of seen
    /// cards, and update each `Opponent`'s possibility list.
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

        for o in self.opponents.values_mut() {
            let certain = o.certain();
            match o.possible_cards.get_mut(&card) {
                Some(1) if certain.not() => {
                    o.possible_cards.remove(&card);
                }
                Some(n) if certain.not() => {
                    *n -= 1;
                }
                _ => {}
            }
        }
    }

    /// Unsee a card that was perhaps added in error from a misclick.
    fn unsee(&mut self, cid: usize, card: Card) {
        self.seen.remove(&cid);

        let entry = self.deck.entry(card).or_insert(0);
        *entry += 1;

        for o in self.opponents.values_mut() {
            let c = o.possible_cards.entry(card).or_insert(0);
            *c += 1;
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
        Msg::Unplay(cid, card) => {
            log!(format!("Unseeing {:?}.", card));
            model.unsee(cid, card);
        }
        Msg::Has(oid, card) => {
            for (i, o) in model.opponents.iter_mut() {
                if *i == oid {
                    o.only_has(card);
                } else {
                    let certain = o.certain();
                    match o.possible_cards.get_mut(&card) {
                        Some(1) if certain.not() => {
                            o.possible_cards.remove(&card);
                        }
                        Some(n) if certain.not() => {
                            *n -= 1;
                        }
                        _ => {}
                    }
                }
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
                o.possible_cards = model.deck.clone();
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
        .map(|(id, o)| view_opponent(*id, o))
        .collect::<Vec<_>>(),]
}

/// Render an `Opponent`.
fn view_opponent(oid: usize, opponent: &Opponent) -> Node<Msg> {
    let probs = opponent.card_probs();

    tr![
        td![
            div![format!("Opponent #{}", oid)],
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
