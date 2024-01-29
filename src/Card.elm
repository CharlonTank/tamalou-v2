module Card exposing (..)

import Element exposing (..)
import Element.Border as Border


type alias Card =
    { suit : Suit
    , rank : Rank
    }


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


displayCard : Card -> Element msg
displayCard card =
    image [ Border.rounded 100, width <| px 128 ] { src = "src/cardImages/" ++ cardToString card ++ ".png", description = cardToString card }


cardToString : Card -> String
cardToString card =
    suitToString card.suit ++ "/" ++ rankToString card.rank


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "Clubs"

        Diamonds ->
            "Diamonds"

        Hearts ->
            "Hearts"

        Spades ->
            "Spades"


rankToString : Rank -> String
rankToString rank =
    case rank of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"


sampleCard : Card
sampleCard =
    { suit = Clubs, rank = Ace }


nonShuffledDeck : List Card
nonShuffledDeck =
    [ { suit = Clubs, rank = Ace }
    , { suit = Clubs, rank = Two }
    , { suit = Clubs, rank = Three }
    , { suit = Clubs, rank = Four }
    , { suit = Clubs, rank = Five }
    , { suit = Clubs, rank = Six }
    , { suit = Clubs, rank = Seven }
    , { suit = Clubs, rank = Eight }
    , { suit = Clubs, rank = Nine }
    , { suit = Clubs, rank = Ten }
    , { suit = Clubs, rank = Jack }
    , { suit = Clubs, rank = Queen }
    , { suit = Clubs, rank = King }
    , { suit = Diamonds, rank = Ace }
    , { suit = Diamonds, rank = Two }
    , { suit = Diamonds, rank = Three }
    , { suit = Diamonds, rank = Four }
    , { suit = Diamonds, rank = Five }
    , { suit = Diamonds, rank = Six }
    , { suit = Diamonds, rank = Seven }
    , { suit = Diamonds, rank = Eight }
    , { suit = Diamonds, rank = Nine }
    , { suit = Diamonds, rank = Ten }
    , { suit = Diamonds, rank = Jack }
    , { suit = Diamonds, rank = Queen }
    , { suit = Diamonds, rank = King }
    , { suit = Hearts, rank = Ace }
    , { suit = Hearts, rank = Two }
    , { suit = Hearts, rank = Three }
    , { suit = Hearts, rank = Four }
    , { suit = Hearts, rank = Five }
    , { suit = Hearts, rank = Six }
    , { suit = Hearts, rank = Seven }
    , { suit = Hearts, rank = Eight }
    , { suit = Hearts, rank = Nine }
    , { suit = Hearts, rank = Ten }
    , { suit = Hearts, rank = Jack }
    , { suit = Hearts, rank = Queen }
    , { suit = Hearts, rank = King }
    , { suit = Spades, rank = Ace }
    , { suit = Spades, rank = Two }
    , { suit = Spades, rank = Three }
    , { suit = Spades, rank = Four }
    , { suit = Spades, rank = Five }
    , { suit = Spades, rank = Six }
    , { suit = Spades, rank = Seven }
    , { suit = Spades, rank = Eight }
    , { suit = Spades, rank = Nine }
    , { suit = Spades, rank = Ten }
    , { suit = Spades, rank = Jack }
    , { suit = Spades, rank = Queen }
    , { suit = Spades, rank = King }
    ]


displayCards : List Card -> Element msg
displayCards cards =
    wrappedRow [ spacing 12 ] (List.map displayCard cards)
