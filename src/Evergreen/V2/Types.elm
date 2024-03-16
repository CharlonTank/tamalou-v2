module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V2.Card
import Lamdera
import Time
import Url


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V2.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type alias FTableHand =
    List Evergreen.V2.Card.FCard


type alias FDrawPile =
    List Evergreen.V2.Card.FCard


type alias DiscardPile =
    List Evergreen.V2.Card.Card


type FPlayerToPlayStatus
    = FWaitingPlayerDraw
    | FPlayerHasDraw Evergreen.V2.Card.FCard


type FGameInProgressStatus
    = FTimerRunning Int
    | FPlayerToPlay Lamdera.SessionId FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded Lamdera.ClientId
    | FGameAlreadyStartedWithoutYou


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , gameFrontend : FGame
    , clientId : Maybe Lamdera.ClientId
    , sessionId : Maybe Lamdera.SessionId
    , urlPath : String
    }


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V2.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    }


type alias BDrawPile =
    List Evergreen.V2.Card.Card


type BPlayerToPlayStatus
    = BWaitingPlayerDraw
    | BPlayerHasDraw Evergreen.V2.Card.Card


type BGameInProgressStatus
    = BTimerRunning Int
    | BPlayerToPlay Lamdera.SessionId BPlayerToPlayStatus


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool
    | BGameEnded Lamdera.ClientId


type alias BGame =
    { urlPath : String
    , status : BGameStatus
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
    | DrawCardFromDiscardPileFrontend
    | ReplaceCardInFrontend Int
    | DoubleCardFrontend Int


type ToBackendActionFromGame
    = ConnectToBackend
    | DrawCardFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawCardFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int


type ToBackend
    = NoOpToBackend
    | ToBackendActionFromGame String ToBackendActionFromGame


type BackendMsgFromGame
    = BeginGameAndDistribute4CardsToEach (List Evergreen.V2.Card.Card)
    | TimerTick Time.Posix


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId Lamdera.SessionId Lamdera.ClientId
    | GotUserDisconnected Lamdera.SessionId Lamdera.ClientId
    | BackendMsgFromGame String BackendMsgFromGame


type ToFrontend
    = NoOpToFrontend
    | UpdateGame FGame
    | GotSessionIdAndClientId Lamdera.SessionId Lamdera.ClientId
