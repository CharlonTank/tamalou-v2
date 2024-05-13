module Evergreen.V52.Player exposing (..)

import Evergreen.V52.Card
import Evergreen.V52.Counter
import Lamdera


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V52.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Evergreen.V52.Counter.Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Evergreen.V52.Counter.Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V52.Card.Power)
    | FPlayerHasDraw Evergreen.V52.Card.FCard
    | FPlayerHasDiscard Evergreen.V52.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus
    | FPlayerDisplayTamalouFailure (List Evergreen.V52.Card.Card) Evergreen.V52.Counter.Counter


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V52.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V52.Card.Power)
    | BPlayerHasDrawn Evergreen.V52.Card.Card
    | BPlayerHasDiscard Evergreen.V52.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
    | BPlayerDisplayTamalouFailure (List Evergreen.V52.Card.Card) Evergreen.V52.Counter.Counter
