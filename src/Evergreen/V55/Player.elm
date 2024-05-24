module Evergreen.V55.Player exposing (..)

import Evergreen.V55.Card
import Evergreen.V55.Counter
import Lamdera


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V55.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type alias CurrentPlayer =
    { name : String
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Evergreen.V55.Counter.Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Evergreen.V55.Counter.Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V55.Card.Power)
    | FPlayerHasDraw Evergreen.V55.Card.FCard
    | FPlayerHasDiscard Evergreen.V55.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus
    | FPlayerDisplayTamalouFailure (List Evergreen.V55.Card.Card) Evergreen.V55.Counter.Counter


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V55.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V55.Card.Power)
    | BPlayerHasDrawn Evergreen.V55.Card.Card
    | BPlayerHasDiscard Evergreen.V55.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
    | BPlayerDisplayTamalouFailure (List Evergreen.V55.Card.Card) Evergreen.V55.Counter.Counter
