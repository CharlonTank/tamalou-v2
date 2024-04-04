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

## Features to be Implemented

Here are some of the features that are yet to be implemented:

[x] : power of Jack
[x] : use power instead of drawing if the last person played a card with power and used the power
[x] : timer at the end to be able to double
[x] : Impossible to double as the tamalouOwner
[x] : power of King
[x] : visibility of other cards
[x] : power of Queen
[ ] : Way to see what cards has been choosen when doubling or for each power
[ ] : Add card animations
[ ] : better way to start a game
[ ] : better way to end a game

## Contributing

Contributions are welcome! Please feel free to submit a pull request.

## Building and Running the Project

To run Tamalou, you need to have `lamdera` installed. Once you have these prerequisites, you can build the project by running `lamdera live` in the terminal.

## Notes
Please note that this project is still a work in progress. The game rules and features are subject to change as development progresses.
