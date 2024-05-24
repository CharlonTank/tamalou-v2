module Evergreen.V54.Player exposing (..)

import Evergreen.V54.Card
import Evergreen.V54.Counter
import Lamdera


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V54.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Evergreen.V54.Counter.Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Evergreen.V54.Counter.Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V54.Card.Power)
    | FPlayerHasDraw Evergreen.V54.Card.FCard
    | FPlayerHasDiscard Evergreen.V54.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus
    | FPlayerDisplayTamalouFailure (List Evergreen.V54.Card.Card) Evergreen.V54.Counter.Counter


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V54.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V54.Card.Power)
    | BPlayerHasDrawn Evergreen.V54.Card.Card
    | BPlayerHasDiscard Evergreen.V54.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
    | BPlayerDisplayTamalouFailure (List Evergreen.V54.Card.Card) Evergreen.V54.Counter.Counter
