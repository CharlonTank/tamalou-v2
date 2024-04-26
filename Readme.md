# Tamalou Card Game

- Tamalou is a multiplayer card game currently under development. The game is being built using Lamdera, a Frontend+Backend Elm framework, and Elm UI for the frontend.

- The game combines elements of memory, speed, and strategy.

## Current State of the Project

- The game is currently a work in progress. The main logic of the game is implemented in src/Frontend.elm and src/Backend.elm. The most used types are defined in src/Types.elm.

- You can try it out here: [Tamalou](https://tamalou-v2.lamdera.app/)

## Rules condensed

- 2 to 5 players.
- The objective is to have the lowest score at the end of the game.
- The score is determined by the sum of the points of the cards in your hand:

  - Ace to 9: Worth 1 to 9 points respectively.
  - 10: Worth 0 points.
  - Jack, Queen, King: Each worth 10 points.

- At any time, if the card on top of the discard pile shares a rank with a card you have, you can "double" by clicking on the card in your table hand. If you are incorrect or if another player acts faster, you draw one additional card as a penalty.
- When you have 5 points or less, you can declare "Tamalou!" to reveal your cards and do a final turn, if you have more than 5 points, you draw a card and play continues.

## Full Game Rules

- The objective is to have the lowest score at the end of the game.
- The game is played with a standard 52-card deck.
- The game can be played with 2 to 5 players.

### Starting the Game

1. At the beginning, you see 2 out of your 4 cards.
2. After 5 seconds, your cards are hidden, and the game begins.
3. The game is turn based, with each player taking a turn.

### Taking a Turn

#### Taking a Turn - Step 1

- At the beginning of your turn you have 5 possibilities:

1. Draw a card from the deck and go to [Step 2](#Taking-a-Turn---Step-2)
2. Draw a card from the discard pile go to [Step 2](#Taking-a-Turn---Step-2)
3. Use the power that has been used by the last player and go to [Step Power](#Taking-a-Turn---Step-Power), check the [precision](#Precision:-Using-the-power-used-before) for more information
4. Say ["Tamalou!"](#tamalou)
5. You can also Double as the ["doubling rules"](#Doubling) still applies, in this case, after doubling, you stay [Step 1](#Taking-a-Turn---Step-1)

#### Taking a Turn - Step 2

- You just drew a card, you have 2 possibilities:

1. Discard the card and go to [Step 3](#Taking-a-Turn---Step-3)
2. Replace the card with one of your cards that will be discarded instead and go to [Step 3](#Taking-a-Turn---Step-3)

#### Taking a Turn - Step 3

- You just discarded a card, you have 2 possibilities:

1. The card discarded has no power, your turn is over.
2. The card discarded has a power, you can use it now, check the [power rules](#Special-Card-Powers) for more information. After using the power, your turn is over.

### Doubling

- At any time, (except if you already said succesfully "Tamalou!"), if the card on top of the discard pile shares a rank with a card you have, you can "double" by clicking on the card in your table hand. If you are incorrect or if another player acts faster, you draw one additional card as a penalty.

### Special Card Powers

- **Jack**: Grants an additional turn.
- **Queen**: Allows switching one of your cards with one of your opponent's.
- **King**: Enables you to look at one of your hidden cards.

#### Precision: Using the power used before

- If the last player used a power from another player, you cannot use the power instead of drawing a card.

### Ending the Game

#### Instant Game End

- The game immediately ends if someone doubles and has no cards left.

#### Tamalou

- If you believe you have the lowest score and it is 5 points or less (see [Ranking and Scoring](#Ranking-and-Scoring)), you can declare "Tamalou!". Your cards are then revealed.

1. If your claim is correct, your cards stay visible, you cannot double but you are immune to the Queen's power, and a final turn is played.

2. If incorrect, your cards are concealed again, you draw an additional card, and play continues.

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

## Animations implemented recently

- [x] Doubling success
- [x] Doubling failure (FIXME: not working because of the way steps of elm-enimator are working - need to find a way to fix it)
- [x] Drawing from the deck
- [x] Drawing from the discard pile
- [x] Discarding a card
- [x] Looking at a card
- [x] Choosing an owned card to switch with the Queen
- [x] Choosing an opponent card to switch with the Queen
- [ ] Tamalou failure

## Known issues

- [ ] Doubling failure animation is not working properly because of the way steps of elm-enimator are working - need to find a way to fix it
- [ ] For now when an action occure during another animation the second animation occuring will be cut off - We need to update parts of the game disposition based on the action occuring

## Bugs fixed recently

- [x] The size of the draw pile is weird on iphone when playing in the application

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
- [ ] Add timers
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
