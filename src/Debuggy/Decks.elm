module Debuggy.Decks exposing (aces, kings, queens)

import Card exposing (Card, Rank(..), Suit(..))


aces : List Card
aces =
    List.repeat 52 { suit = Clubs, rank = Ace, show = False }


kings : List Card
kings =
    List.repeat 52 { suit = Clubs, rank = King, show = False }


queens : List Card
queens =
    List.repeat 52 { suit = Clubs, rank = Queen, show = False }
