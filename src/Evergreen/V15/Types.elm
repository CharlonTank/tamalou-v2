module Evergreen.V15.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V15.Card
import Lamdera
import Random
import Time
import Ui
import Url
import Utils.Ui as Ui


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V15.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V15.Card.Card
    }


type alias FTableHand =
    List Evergreen.V15.Card.FCard


type alias FDrawPile =
    List Evergreen.V15.Card.FCard


type alias DiscardPile =
    List Evergreen.V15.Card.Card


type Counter
    = Five
    | Four
    | Three
    | Two
    | One
    | Zero


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V15.Card.Power)
    | FPlayerHasDraw Evergreen.V15.Card.FCard
    | FPlayerHasDiscard Evergreen.V15.Card.Power
    | FPlayerLookACard (Maybe Counter)


type FGameInProgressStatus
    = FStartTimerRunning Counter
    | FPlayerToPlay Lamdera.SessionId FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus
    | FEndTimerRunning Counter


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded Lamdera.ClientId
    | FGameAlreadyStartedWithoutYou


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , device : Ui.Device
    , gameFrontend : FGame
    , clientId : Maybe Lamdera.ClientId
    , sessionId : Maybe Lamdera.SessionId
    , urlPath : String
    , errors : List String
    , admin : Bool
    }


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V15.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type alias BDrawPile =
    List Evergreen.V15.Card.Card


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V15.Card.Power)
    | BPlayerHasDraw Evergreen.V15.Card.Card
    | BPlayerHasDiscard Evergreen.V15.Card.Power
    | BPlayerLookACard (Maybe Counter)


type BGameInProgressStatus
    = BStartTimerRunning Counter
    | BPlayerToPlay Lamdera.SessionId BPlayerToPlayStatus
    | BEndTimerRunning Counter


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded Lamdera.SessionId


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , seed : Random.Seed
    }


type alias BackendModel =
    { games : List BGame
    , errors : List String
    , admins : List Lamdera.SessionId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | DrawCardFromDeckFrontend
    | TamalouFrontend
    | DiscardCardFrontend
    | DrawFromDiscardPileFrontend
    | PowerIsUsedFrontend
    | PowerPassFrontend
    | ReplaceCardInFrontend Int
    | DoubleCardFrontend Int
    | LookAtCardFrontend Int
    | GotWindowSize Ui.Device


type ActionFromGameToBackend
    = ConnectToBackend
    | DrawCardFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int
    | LookAtCardInTableHandToBackend Int
    | PowerIsUsedToBackend
    | PowerIsNotUsedToBackend
    | TamalouToBackend


type ToBackend
    = NoOpToBackend
    | ActionFromGameToBackend String ActionFromGameToBackend
    | ConnectToAdminToBackend


type BackendMsgFromGame
    = TimerTick Time.Posix
    | BeginGameAndDistribute4CardsToEach (List Evergreen.V15.Card.Card)


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId Lamdera.SessionId Lamdera.ClientId
    | GotUserDisconnected Lamdera.SessionId Lamdera.ClientId
    | BackendMsgFromGame String BackendMsgFromGame


type ToFrontend
    = NoOpToFrontend
    | UpdateAdminToFrontend (List String)
    | UpdateGameToFrontend FGame
    | GotSessionIdAndClientIdToFrontend Lamdera.SessionId Lamdera.ClientId
