module Card exposing (..)

import Element exposing (..)
import Element.Border as Border


type alias Card =
    { suit : Suit
    , rank : Rank
    , show : Bool
    }


type FCard
    = FaceUp Card
    | FaceDown


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


toString : Card -> String
toString card =
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
    { suit = Clubs, rank = Ace, show = False }


nonShuffledDeck : List Card
nonShuffledDeck =
    [ { suit = Clubs, rank = Ace, show = False }
    , { suit = Clubs, rank = Two, show = False }
    , { suit = Clubs, rank = Three, show = False }
    , { suit = Clubs, rank = Four, show = False }
    , { suit = Clubs, rank = Five, show = False }
    , { suit = Clubs, rank = Six, show = False }
    , { suit = Clubs, rank = Seven, show = False }
    , { suit = Clubs, rank = Eight, show = False }
    , { suit = Clubs, rank = Nine, show = False }
    , { suit = Clubs, rank = Ten, show = False }
    , { suit = Clubs, rank = Jack, show = False }
    , { suit = Clubs, rank = Queen, show = False }
    , { suit = Clubs, rank = King, show = False }
    , { suit = Diamonds, rank = Ace, show = False }
    , { suit = Diamonds, rank = Two, show = False }
    , { suit = Diamonds, rank = Three, show = False }
    , { suit = Diamonds, rank = Four, show = False }
    , { suit = Diamonds, rank = Five, show = False }
    , { suit = Diamonds, rank = Six, show = False }
    , { suit = Diamonds, rank = Seven, show = False }
    , { suit = Diamonds, rank = Eight, show = False }
    , { suit = Diamonds, rank = Nine, show = False }
    , { suit = Diamonds, rank = Ten, show = False }
    , { suit = Diamonds, rank = Jack, show = False }
    , { suit = Diamonds, rank = Queen, show = False }
    , { suit = Diamonds, rank = King, show = False }
    , { suit = Hearts, rank = Ace, show = False }
    , { suit = Hearts, rank = Two, show = False }
    , { suit = Hearts, rank = Three, show = False }
    , { suit = Hearts, rank = Four, show = False }
    , { suit = Hearts, rank = Five, show = False }
    , { suit = Hearts, rank = Six, show = False }
    , { suit = Hearts, rank = Seven, show = False }
    , { suit = Hearts, rank = Eight, show = False }
    , { suit = Hearts, rank = Nine, show = False }
    , { suit = Hearts, rank = Ten, show = False }
    , { suit = Hearts, rank = Jack, show = False }
    , { suit = Hearts, rank = Queen, show = False }
    , { suit = Hearts, rank = King, show = False }
    , { suit = Spades, rank = Ace, show = False }
    , { suit = Spades, rank = Two, show = False }
    , { suit = Spades, rank = Three, show = False }
    , { suit = Spades, rank = Four, show = False }
    , { suit = Spades, rank = Five, show = False }
    , { suit = Spades, rank = Six, show = False }
    , { suit = Spades, rank = Seven, show = False }
    , { suit = Spades, rank = Eight, show = False }
    , { suit = Spades, rank = Nine, show = False }
    , { suit = Spades, rank = Ten, show = False }
    , { suit = Spades, rank = Jack, show = False }
    , { suit = Spades, rank = Queen, show = False }
    , { suit = Spades, rank = King, show = False }
    ]


handScore : List Card -> Int
handScore hand =
    List.foldl
        (\card acc ->
            case card.rank of
                Ace ->
                    acc + 1

                Two ->
                    acc + 2

                Three ->
                    acc + 3

                Four ->
                    acc + 4

                Five ->
                    acc + 5

                Six ->
                    acc + 6

                Seven ->
                    acc + 7

                Eight ->
                    acc + 8

                Nine ->
                    acc + 9

                Ten ->
                    acc + 0

                Jack ->
                    acc + 10

                Queen ->
                    acc + 10

                King ->
                    acc + 10
        )
        0
        hand


handIsLessThanFive : List Card -> Bool
handIsLessThanFive tableHand =
    handScore tableHand <= 5
