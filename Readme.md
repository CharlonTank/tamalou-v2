# Tamalou Card Game

Tamalou is a multiplayer card game currently under development. The game is being built using Lamdera, a Frontend+Backend Elm framework, and Elm UI for the frontend.

## Current State of the Project

The game is currently a work in progress. The main logic of the game is implemented in src/Frontend.elm and src/Backend.elm. The most used types are defined in src/Types.elm.

You can try it out here: [Tamalou](https://tamalou-v2.lamdera.app/)

## Game Rules

The game combines elements of memory, speed, and strategy.

### Starting the Game

1. At the beginning, you see 2 out of 4 of your cards.
2. You have the option to draw a card from either the deck or the discard pile.
3. You can play this card by either clicking on it or by clicking on one of your existing cards.

### Special Card Powers

- **Jack**: Grants an additional turn.
- **Queen**: Allows switching one of your cards with one of your opponent's.
- **King**: Enables you to look at one of your hidden cards.

### Doubling

- At any point, if the card on top of the discard pile shares a rank with a card you have, you can "double" by clicking on the card in your table hand. If you are incorrect or if another player acts faster, you draw one additional card.

### Ending the Game

#### Instant Game End

- The game immediately ends if someone doubles and has no cards left.

#### Tamalou

- If you believe you have the lowest score and it is 5 points or less, you can declare "Tamalou". Your cards are then revealed. If your claim is correct, your cards stay visible, immune to the Queen's power, and a final turn is played without the ability to double. If incorrect, your cards are concealed again, you draw an additional card, and play continues.

### Ranking and Scoring

- **Card Points**:

  - Ace to 9: Worth 1 to 9 points respectively.
  - 10: Worth 0 points.
  - Jack, Queen, King: Each worth 10 points.

- **Determining the Winner**:
  - The player with the lowest score wins.
  - In the event of a tie, the player with fewer cards wins.
  - If there's still a tie, the player who declared "Tamalou" wins.

## Features implemented recently

Here are some of the features that are yet to be implemented:

- [x] Power of Jack
- [x] Use power instead of drawing if the last person played a card with power and used the power
- [x] Timer at the end to be able to double
- [x] Impossible to double as the tamalouOwner
- [x] Power of King
- [x] Visibility of other cards
- [x] Power of Queen
- [x] Way to see what cards has been choosen when doubling or for each power

## Add card animations in progress

- [x] Doubling success
- [x] Doubling failure (FIXME: not working because of the way steps of elm-enimator are working - need to find a way to fix it)
- [x] Drawing from the deck
- [x] Drawing from the discard pile
- [x] Discarding a card
- [ ] Looking at a card
- [ ] Choosing an owned card to switch with the Queen
- [ ] Choosing an opponent card to switch with the Queen
- [ ] Tamalou failure

## Known issues

- [ ] Doubling failure animation is not working because of the way steps of elm-enimator are working - need to find a way to fix it
- [ ] The size of the draw pile is weird on iphone when playing in the application
- [ ] For now when an action occure during another animation the second animation occuring will be cut off - We need to update parts of the game disposition based on the action occuring

<!-- own notes:
p1 card draw: animation of 2000 ms
at 1000ms, p2 double: animation of 2000 ms

state 0ms -> carte sur le dessus de la drawpile, p1 joue une carte, la carte commence à bouger vers le centre.
-- state 0ms: on a un nouvel etat à 0ms mais en "attente" car on attend l'animation de p1 qu'elle finisse avant d'updater avec le nouveau state

state 1000ms -> la carte continue de bouger vers le centre, elle est à 50% de la distance, p2 double avec une carte, la carte continue de bouger vers le centre et la deuxieme carte commence à bouger vers la discard pile.
-- state 1000ms: on a un nouvel etat à 1000ms mais en "attente" car on attend l'animation de p1 qu'elle finisse avant d'updater avec le nouveau state

state 2000ms -> la carte de p1 à fini de bouger vers le centre, p2 est toujours en train de jouer sa carte, la carte de p2 est à 50% de la distance.
-- state 2000ms: l'état reçu à 0ms avec la carte de p1 en moins dans sa main doit être pris en compte et update le frontend state, le problème c'est que si on fait ça, la carte de p2 va reaparaitre avant que à 3000ms la carte redisparraisse grace au state reçu à 1000ms. -->

## Features to be implemented

- [ ] Add rules pages
- [ ] Better way to start a game
- [ ] Better way to end a game
- [ ] Add more animations to make the game more appealing
- [ ] Add authentification
- [ ] Add a elo system
- [ ] Add a matchmaking system

## Contributing

Contributions are welcome! Please feel free to submit a pull request.

## Building and Running the Project

To run Tamalou, you need to have `lamdera` installed. Once you have these prerequisites, you can build the project by running `lamdera live` in the terminal.

## Notes

Please note that this project is still a work in progress. The game rules and features are subject to change as development progresses.
