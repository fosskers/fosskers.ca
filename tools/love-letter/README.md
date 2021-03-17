# Love Letter

This is an information tracker for the game *Love Letter*, written in Rust and
compiled to WASM.

## Spec

Below, *user* refers to the person using the Tracker, *opponent* refers to any
other player whose data is being tracked, while *player* refers to both person
and opponent collectively.

### UI Notes

A reset button must be available to restart the tracker.

Notify the user when it's their last turn (based on remaining cards in the
deck).

### Initial Setup

It is assumed that there is always a user with a card or two visible in their
hand. (i.e. the tracker is not meant for 4 players with hidden hands).

The `Model` must keep track of:

- how many cards are left in the deck.
- which cards have been seen, and which are left.
- which players are alive and dead.
- which cards each player has played.
- a "card probability map" for each player.

The user experiences the following:

1. The user must be asked what card they were initially given.
2. The user is then asked who was selected to go first, and then play proceeds
   as usual.

### A Single Turn - Opponent's Turn

1. A card is dealt to the opponent whose turn it is.
2. Remove "invulnerable" status.
3. Ask the user which card the opponent played, and resolve its effects.
4. **Game end check.**
5. If the game didn't end, automatically move to the next player's turn.

### A Single Turn - User's Turn

1. Ask the user to declare what card they were drawn.
2. Remove "invulnerable" status.
3. Update all probabilities.
4. Automatically resolve "auto-plays" like Prince+Countess.
5. Otherwise, user clicks one of their cards to play and its effects are resolved.
6. **Game end check.**
7. If the game didn't end, automatically move to the next player's turn.

### Card Resolution

At the end of each card resolution, update all probabilities. If a probability
update ever results in a 100% chance of Princess on a opponent, highlight their
zone.

#### Guard

The same for user or opponent.

1. Choose a target player.
2. Choose a card to search for.
3. If they didn't die, update that opponent's probability map to exclude the searched card.
   - Note: this would affect the *other* opponents' probabilities too, would it not?
4. If they did die, take them out of the turn order.

#### Priest

If opponent, do nothing.

If user, ask them what card was revealed.

#### Baron

If opponent -> opponent:

1. Choose a target.
2. Who died?
3. What did they drop?
4. Update the killer's probabilities specially.

If opponent -> user:

1. Choose user.
2. If opponent died, what did they drop?
3. If user died, **game over.**

If user -> opponent:

1. Choose target.
2. If opponent died, what did they drop?
3. If user died, **game over.**

#### Handmaid

Mark the user/opponent who played this as "invulnerable". This prevents the user
from targetting that opponent during their card play step, or when reporting the
target of an opponent's card.

#### Prince

The same for user or opponent.

1. Choose a target player.
2. Ask which card was discarded.
3. If princess, that player is dead and is removed from the turn order.

#### King

If opponent:

1. Choose target.
2. If user was the target, update the opponent's probability map to 100% certainty.
3. Otherwise, swap the probably maps of the two opponents.

If user:

1. Choose target.
2. Update the opponent's probability map to 100% certainty.

#### Countess

If opponent, do nothing (but consider the probably updates specially).

If user, do nothing.

#### Princess

The player who discards this is dead.
