module Evergreen.V12.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V12.Card
import Lamdera
import Random
import Time
import Ui
import Url
import Utils.Ui as Ui


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V12.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V12.Card.Card
    }


type alias FTableHand =
    List Evergreen.V12.Card.FCard


type alias FDrawPile =
    List Evergreen.V12.Card.FCard


type alias DiscardPile =
    List Evergreen.V12.Card.Card


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V12.Card.Power)
    | FPlayerHasDraw Evergreen.V12.Card.FCard
    | FPlayerHasDiscard Evergreen.V12.Card.Power


type FGameInProgressStatus
    = FTimerRunning Int
    | FPlayerToPlay Lamdera.SessionId FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus


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
    }


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V12.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type alias BDrawPile =
    List Evergreen.V12.Card.Card


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V12.Card.Power)
    | BPlayerHasDraw Evergreen.V12.Card.Card
    | BPlayerHasDiscard Evergreen.V12.Card.Power


type BGameInProgressStatus
    = BTimerRunning Int
    | BPlayerToPlay Lamdera.SessionId BPlayerToPlayStatus


type PowerAlreadyUsed
    = PowerAlreadyUsed Bool


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool PowerAlreadyUsed
    | BGameEnded Lamdera.SessionId


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , seed : Random.Seed
    }


type alias BackendModel =
    { games : List BGame
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
    | GotWindowSize Ui.Device


type ToBackendActionFromGame
    = ConnectToBackend
    | DrawCardFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawOrUsePowerFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int
    | PowerIsUsedToBackend
    | PowerIsNotUsedToBackend
    | TamalouToBackend


type ToBackend
    = NoOpToBackend
    | ToBackendActionFromGame String ToBackendActionFromGame


type BackendMsgFromGame
    = TimerTick Time.Posix
    | BeginGameAndDistribute4CardsToEach (List Evergreen.V12.Card.Card)


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId Lamdera.SessionId Lamdera.ClientId
    | GotUserDisconnected Lamdera.SessionId Lamdera.ClientId
    | BackendMsgFromGame String BackendMsgFromGame


type ToFrontend
    = NoOpToFrontend
    | UpdateGame FGame
    | GotSessionIdAndClientId Lamdera.SessionId Lamdera.ClientId
