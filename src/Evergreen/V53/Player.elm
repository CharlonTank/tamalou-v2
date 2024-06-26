module Evergreen.V53.Player exposing (..)

import Evergreen.V53.Card
import Evergreen.V53.Counter
import Lamdera


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V53.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Evergreen.V53.Counter.Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Evergreen.V53.Counter.Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V53.Card.Power)
    | FPlayerHasDraw Evergreen.V53.Card.FCard
    | FPlayerHasDiscard Evergreen.V53.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus
    | FPlayerDisplayTamalouFailure (List Evergreen.V53.Card.Card) Evergreen.V53.Counter.Counter


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V53.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V53.Card.Power)
    | BPlayerHasDrawn Evergreen.V53.Card.Card
    | BPlayerHasDiscard Evergreen.V53.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
    | BPlayerDisplayTamalouFailure (List Evergreen.V53.Card.Card) Evergreen.V53.Counter.Counter
