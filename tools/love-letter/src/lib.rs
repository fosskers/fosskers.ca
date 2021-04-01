use itertools::Itertools;
use seed::{prelude::*, *};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Not;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

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
        ALL_CARDS.iter().filter(|c| c <= &&card).for_each(|c| {
            self.nots.insert(*c);
        })
    }

    /// An `Opponent` played a specific card and special circumstances must be
    /// considered when updating the knowledge we have on them.
    fn played(&mut self, card: Card) {
        // If they played a card that we know they didn't have until this draw,
        // then we knew that they just drew it. Thus, they still can't have any
        // of those cards.
        //
        // Otherwise, all our "not" knowledge is stale.
        if self.nots.contains(&card).not() {
            self.nots.clear();
        }

        // Regardless of what other "not" knowledge became stale, if they just
        // played a Prince or a King, we know they can't have a Countess.
        match card {
            Card::Prince | Card::King => {
                self.nots.insert(Card::Countess);
            }
            _ => {}
        }

        // If they played a card that we 100% knew they had, then we can't know
        // that they still have it (even if they just drew a second copy).
        if self.has == Some(card) {
            self.has.take();
        }
    }

    /// If we know they don't have every other card but one, then we can add
    /// that knowledge with confidence.
    fn no_other_choice(&mut self, deck: &BTreeMap<Card, usize>) {
        let mut poss = self.possibles(deck);

        if poss.len() == 1 {
            self.has = poss.drain().next();
            self.nots.clear();
        }
    }

    /// What cards could this `Opponent` potentially have?
    fn possibles(&self, deck: &BTreeMap<Card, usize>) -> HashSet<Card> {
        deck.keys()
            .filter(|c| self.nots.contains(c).not())
            .map(|c| *c)
            .collect()
    }
}

/// The global state of the tracker.
struct Model {
    /// Current string in the "Opponent Name" input field.
    name_input: Option<String>,
    /// Opponent names.
    names: Vec<String>,
    /// Cards that haven't been seen.
    tracker: Vec<Card>,
    /// Cards that have been **played** by the players.
    seen: BTreeMap<Card, usize>,
    /// The possible cards remaining.
    deck: BTreeMap<Card, usize>,
    /// The other players.
    opponents: BTreeMap<usize, Opponent>,
}

impl Model {
    fn new() -> Model {
        Model {
            name_input: None,
            names: vec!["Sam".to_string(), "Merry".to_string(), "Pippin".to_string()],
            tracker: Vec::new(),
            seen: BTreeMap::new(),
            deck: Card::full_deck(),
            opponents: BTreeMap::new(),
        }
    }

    fn reset(&mut self) {
        let mut opponents = BTreeMap::new();
        for (i, n) in self.names.iter().enumerate() {
            opponents.insert(i + 1, Opponent::new(n.clone()));
        }

        self.tracker = Vec::new();
        self.seen = BTreeMap::new();
        self.deck = Card::full_deck();
        self.opponents = opponents;
    }

    /// A new concrete card has been seen, so add it to the master list of seen
    /// cards.
    fn seen(&mut self, card: Card) {
        let entry = self.seen.entry(card).or_insert(0);
        *entry += 1;

        match self.deck.get_mut(&card) {
            Some(n) if *n > 0 => {
                *n -= 1;
            }
            _ => {}
        }
    }

    /// Unsee a card that was perhaps added in error from a misclick.
    fn unsee(&mut self, card: Card) {
        let entry = self.deck.entry(card).or_insert(0);
        *entry += 1;

        match self.seen.get_mut(&card) {
            Some(n) if *n > 0 => {
                *n -= 1;
            }
            _ => {}
        }
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
                let notted: usize = o.nots.iter().filter_map(|c| self.deck.get(&c)).sum();
                let rem = (self.deck.values().sum::<usize>()
                    - certains.values().sum::<usize>()
                    - notted) as f32;

                // BELIEVE THE DECK.
                ALL_CARDS
                    .iter()
                    .map(|c| match self.deck.get(c) {
                        _ if o.nots.contains(c) => (*c, 0.0),
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
    /// The user types something in the "Opponent Name" input box.
    Input(String),
    /// Commit a new player name.
    Add,
    /// Remove a player name.
    Remove(usize),
    /// Remove all player names.
    RemoveAll,
    /// Begin the game.
    Start,
    /// Set the tracker state to its initial... state.
    Reset,
    /// Forget special knowledge for a particular player.
    ResetPlayer(usize),
    /// A card was seen.
    Seen(Card),
    /// Mark a seen card as unseen, perhaps if a misclick was made.
    Unsee(Card),
    /// A card was played by a specific player.
    Played(usize, Card),
    /// A "Guard miss" occurred.
    Guard(usize, Card),
    /// A Priest was played.
    Priest(usize, Card),
    /// A Baron was played.
    Baron(usize, Card),
    /// A King swap occurred between two Opponents.
    King(usize, usize),
    /// Note that a player died. They should be removed from the tracker.
    Kill(usize),
}

fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model::new()
}

fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::Input(s) => {
            model.name_input.replace(s);
        }
        Msg::Add => match model.name_input.take().map(|s| s.trim().to_owned()) {
            Some(s) if s.is_empty().not() => model.names.push(s),
            _ => {}
        },
        Msg::Remove(ix) => {
            if ix < model.names.len() {
                model.names.remove(ix);
            }
        }
        Msg::RemoveAll => model.names.clear(),
        Msg::Start => {
            if model.names.is_empty().not() {
                model.reset();
            }
        }
        Msg::Reset => model.reset(),
        Msg::Seen(card) => model.seen(card),
        Msg::Unsee(card) => model.unsee(card),
        Msg::Played(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.played(card);
                o.no_other_choice(&model.deck);
            }
            update(Msg::Seen(card), model, orders);
        }
        Msg::Guard(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.nots.insert(card);
                o.no_other_choice(&model.deck);
            }
        }
        Msg::Priest(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.only_has(card);
            }
        }
        Msg::Baron(oid, card) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.baron(card);
                o.no_other_choice(&model.deck);
            }
        }
        Msg::King(oid1, oid2) => {
            let m1 = {
                model.opponents.get_mut(&oid1).map(|o| {
                    let has = o.has.take();
                    let not = o.nots.drain().collect::<HashSet<_>>();

                    (has, not)
                })
            };

            let m2 = {
                model.opponents.get_mut(&oid2).map(|o| {
                    let has = o.has.take();
                    let not = o.nots.drain().collect::<HashSet<_>>();

                    (has, not)
                })
            };

            match (m1, m2) {
                (Some((has1, not1)), Some((has2, not2))) => {
                    if let Some(o2) = model.opponents.get_mut(&oid2) {
                        o2.has = has1;
                        o2.nots = not1;
                    }

                    if let Some(o1) = model.opponents.get_mut(&oid1) {
                        o1.has = has2;
                        o1.nots = not2;
                    }
                }
                // TODO Edge cases. Give back the data.
                _ => {}
            }
        }
        Msg::Kill(oid) => {
            model.opponents.remove(&oid);
        }
        Msg::ResetPlayer(oid) => {
            if let Some(o) = model.opponents.get_mut(&oid) {
                o.has.take();
                o.nots.clear();
            }
        }
    }
}

fn view(model: &Model) -> Node<Msg> {
    match model.opponents.len() {
        0 => view_startup(model),
        _ => view_game(model),
    }
}

fn view_startup(model: &Model) -> Node<Msg> {
    div![
        C!["grid-startup-container"],
        div![C!["grid-startup-main"], view_startup_main(model)],
        div![C!["grid-startup-footer"], view_credit_footer()]
    ]
}

fn view_startup_main(model: &Model) -> Node<Msg> {
    div![
        C!["startup-main"],
        div![
            C!["has-text-centered"],
            p![
                C!["title", "is-1", "love-letter-title"],
                "Love Letter Tracker"
            ],
            p![
                C!["subtitle", "is-6"],
                em!["Track card knowledge in a game of Love Letter."]
            ]
        ],
        view_links(),
        view_opponent_select(model),
        div![
            C!["buttons", "are-large", "has-addons"],
            button![
                C!["button", "is-light", "is-outlined"],
                span!["Clear"],
                span![C!["icon"], i![C!["fas", "fa-times"]]],
                ev(Ev::Click, |_| Msg::RemoveAll)
            ],
            button![
                C!["button", "is-success"],
                span!["Start"],
                span![C!["icon"], i![C!["fas", "fa-crown"]]],
                ev(Ev::Click, |_| Msg::Start)
            ]
        ]
    ]
}

fn view_links() -> Node<Msg> {
    div![
        C!["blue-link"],
        a![attrs! { At::Href => "TODO"}, "How to Use"],
        "・",
        a![
            attrs! { At::Href => "https://github.com/fosskers/fosskers.ca/issues"},
            "Report Bug"
        ],
        "・",
        a![
            attrs! { At::Href => "https://www.buymeacoffee.com/fosskers"},
            "Donate"
        ],
        "・",
        a![
            C!["gold"],
            attrs! { At::Href => "https://www.asmodee-digital.com/en/love-letter/"},
            "Buy Love Letter"
        ],
    ]
}

fn view_opponent_select(model: &Model) -> Vec<Node<Msg>> {
    vec![
        div![C!["block", "bold-silver"], "Opponents"],
        div![
            C!["field", "has-addons"],
            div![
                C!["control"],
                input![
                    C!["input"],
                    attrs! {
                        At::Type => "text",
                        At::Placeholder => "Opponent Name",
                        At::Value => model.name_input.as_ref().map(|s| s.as_str()).unwrap_or(""),
                    },
                    input_ev(Ev::Input, Msg::Input),
                    keyboard_ev(Ev::KeyDown, |event| {
                        (event.key() == "Enter").then(|| Msg::Add)
                    })
                ]
            ],
            div![
                C!["control"],
                a![
                    C!["button", "is-success"],
                    span![C!["icon"], i![C!["fas fa-user-plus"]]],
                    ev(Ev::Click, |_| Msg::Add)
                ]
            ]
        ],
        div![
            C!["field", "is-grouped"],
            model
                .names
                .iter()
                .enumerate()
                .map(|(i, n)| p![
                    C!["control"],
                    button![
                        C!["button", "is-link", "is-light"],
                        span![n],
                        span![C!["icon"], i![C!["fas", "fa-times"]]],
                        ev(Ev::Click, move |_| Msg::Remove(i))
                    ]
                ])
                .collect::<Vec<_>>()
        ],
    ]
}

fn view_credit_footer() -> Node<Msg> {
    footer![
        C!["footer"],
        div![C!["tracker-version"], env!("CARGO_PKG_VERSION")],
        div![
            C!["credits", "bold-silver", "right-align", "blue-link"],
            div![
                "Love Letter by ",
                a![
                    attrs! { At::Href => "http://kanaifactory.web.fc2.com/menu.html"},
                    "Kanai Factory"
                ],
                "."
            ],
            div![
                "Tracker created by ",
                a![attrs! { At::Href => "https://github.com/fosskers"}, "Colin"],
                " via ",
                a![attrs! { At::Href => "https://www.rust-lang.org/"}, "Rust"],
                " and ",
                a![attrs! { At::Href => "https://seed-rs.org/"}, "Seed"],
                "."
            ],
            div!["Thanks to John, Sebastian, and Dan for testing."]
        ],
    ]
}

fn view_game(model: &Model) -> Node<Msg> {
    div![
        C!["grid-game-container"],
        div![
            C!["grid-game-top-left", "tracker-version"],
            env!("CARGO_PKG_VERSION")
        ],
        div![C!["grid-game-top-center"], view_top_bar()],
        div![C!["grid-game-top-right"], view_reset_button()],
        div![C!["grid-game-unseen"], view_card_choice(model)],
        div![C!["grid-game-seen"], view_seen_cards(model)],
        div![C!["grid-game-opponents"], view_player_grid(model)]
    ]
}

fn view_top_bar() -> Node<Msg> {
    div![
        C!["top-bar"],
        span![C!["title", "love-letter-title"], "Love Letter Tracker"],
    ]
}

fn view_reset_button() -> Node<Msg> {
    div![
        C!["reset-button"],
        button![
            C!["button", "is-warning"],
            span!["Reset Game"],
            span![C!["icon"], i![C!["fas", "fa-redo-alt"]]],
            ev(Ev::Click, |_| Msg::Reset)
        ]
    ]
}

fn view_card_choice(model: &Model) -> Vec<Node<Msg>> {
    vec![
        div![C!["bold-silver"], "Remaining Unseen Cards"],
        div![
            C!["card-line"],
            ALL_CARDS
                .iter()
                .map(|c| match model.deck.get(c) {
                    None | Some(0) => {
                        div![img![
                            C!["rounded-image", "zero"],
                            attrs! {At::Src => c.image()}
                        ]]
                    }
                    Some(n) => {
                        let card = c.clone();
                        div![
                            C!["text-overlay"],
                            input![
                                attrs! {At::Type => "image", At::Src => c.image()},
                                ev(Ev::Click, move |_| Msg::Seen(card))
                            ],
                            (*n > 1).then(|| span![C!["tag", "is-dark", "is-rounded"], "x", n])
                        ]
                    }
                })
                .collect::<Vec<_>>()
        ],
    ]
}

fn view_seen_cards(model: &Model) -> Vec<Node<Msg>> {
    vec![
        div![C!["bold-silver"], "Seen Cards"],
        div![
            C!["card-line"],
            ALL_CARDS.iter().map(|c| match model.seen.get(c) {
                None | Some(0) => {
                    div![img![
                        C!["rounded-image", "zero"],
                        attrs! {At::Src => c.image()}
                    ]]
                }
                Some(n) => {
                    let card = c.clone();
                    div![
                        C!["text-overlay"],
                        input![
                            attrs! { At::Type => "image", At::Src => card.image()},
                            ev(Ev::Click, move |_| Msg::Unsee(card))
                        ],
                        (*n > 1).then(|| span![C!["tag", "is-dark", "is-rounded"], "x", n])
                    ]
                }
            })
        ],
    ]
}

fn view_player_grid(model: &Model) -> Node<Msg> {
    div![model
        .opponents
        .iter()
        .map(|(id, o)| view_opponent(model, *id, o))
        .collect::<Vec<_>>(),]
}

/// Render an `Opponent`.
fn view_opponent(model: &Model, oid: usize, opponent: &Opponent) -> Node<Msg> {
    let probs = model.probs(opponent);

    div![
        C!["opponent-row"],
        div![
            div![C!["opponent-name"], &opponent.name],
            div![
                C!["buttons", "has-addons"],
                button![
                    C!["button", "is-danger"],
                    (model.opponents.len() < 2).then(|| attrs! {At::Disabled => ""}),
                    span!["Kill"],
                    span![C!["icon"], i![C!["fas", "fa-skull"]]],
                    ev(Ev::Click, move |_| Msg::Kill(oid))
                ],
                button![
                    C!["button"],
                    span!["Forget"],
                    span![C!["icon"], i![C!["fas", "fa-redo-alt"]]],
                    ev(Ev::Click, move |_| Msg::ResetPlayer(oid))
                ],
            ],
            match model.opponents.len() {
                0 | 1 => {
                    div![]
                }
                _ => {
                    div![
                        C!["buttons", "has-addons"],
                        button![
                            C!["button", "is-success"],
                            span![C!["icon"], i![C!["fas", "fa-crown"]]]
                        ],
                        model
                            .opponents
                            .iter()
                            .filter(|(id, _)| **id != oid)
                            .map(|(id, o)| {
                                let id = *id;
                                let name: String = o.name.chars().take(3).collect();
                                button![
                                    C!["button", "is-success", "is-outlined"],
                                    format!("{}", name),
                                    ev(Ev::Click, move |_| Msg::King(oid, id))
                                ]
                            })
                            .collect::<Vec<_>>()
                    ]
                }
            }
        ],
        probs
            .into_iter()
            .map(|(card, prob)| div![
                C!["opponent-card"],
                div![
                    C!["text-overlay"],
                    input![
                        C![(prob == 0.0).then(|| "zero")],
                        attrs! {
                            At::Type => "image",
                            At::Src => card.image()
                        },
                        match model.deck.get(&card) {
                            None | Some(0) => None,
                            _ => Some(ev(Ev::Click, move |_| Msg::Played(oid, card))),
                        }
                    ],
                    (prob > 0.0).then(|| span![
                        C!["tag", "is-dark", "is-medium", "is-rounded"],
                        format!("{:.1}%", prob)
                    ])
                ],
                div![
                    C!["buttons", "has-addons"],
                    button![
                        C!["button", "is-warning"],
                        span![C!["icon"], i![C!["fas", "fa-times"]]],
                        ev(Ev::Click, move |_| Msg::Guard(oid, card))
                    ],
                    button![
                        C!["button", "is-danger"],
                        span![C!["icon"], i![C!["fas", "fa-eye"]]],
                        ev(Ev::Click, move |_| Msg::Priest(oid, card))
                    ],
                    button![
                        C!["button", "is-link"],
                        span![C!["icon"], i![C!["fas", "fa-fan"]]],
                        ev(Ev::Click, move |_| Msg::Baron(oid, card))
                    ]
                ]
            ])
            .collect::<Vec<_>>()
    ]
}

#[wasm_bindgen(start)]
pub fn start() {
    App::start("app", init, update, view);
}
