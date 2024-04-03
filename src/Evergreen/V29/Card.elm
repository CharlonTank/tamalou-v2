module Evergreen.V29.Card exposing (..)


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias Card =
    { suit : Suit
    , rank : Rank
    , show : Bool
    }


type FCard
    = FaceUp Card
    | FaceDown


type Power
    = PlayAgain
    | Switch2Cards
    | LookACard
