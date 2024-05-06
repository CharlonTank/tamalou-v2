module Evergreen.V51.Player exposing (..)

import Evergreen.V51.Card
import Evergreen.V51.Counter
import Lamdera


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V51.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Evergreen.V51.Counter.Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Evergreen.V51.Counter.Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V51.Card.Power)
    | FPlayerHasDraw Evergreen.V51.Card.FCard
    | FPlayerHasDiscard Evergreen.V51.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V51.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V51.Card.Power)
    | BPlayerHasDrawn Evergreen.V51.Card.Card
    | BPlayerHasDiscard Evergreen.V51.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
