module Evergreen.V49.Player exposing (..)

import Evergreen.V49.Card
import Evergreen.V49.Counter
import Lamdera


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V49.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Evergreen.V49.Counter.Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Evergreen.V49.Counter.Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V49.Card.Power)
    | FPlayerHasDraw Evergreen.V49.Card.FCard
    | FPlayerHasDiscard Evergreen.V49.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V49.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V49.Card.Power)
    | BPlayerHasDrawn Evergreen.V49.Card.Card
    | BPlayerHasDiscard Evergreen.V49.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
