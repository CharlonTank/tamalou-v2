module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Card exposing (Card, FCard)
import Lamdera exposing (ClientId, SessionId)
import Time exposing (Posix)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , gameFrontend : FGame
    , clientId : Maybe ClientId
    , sessionId : Maybe SessionId
    , urlPath : String
    }


type alias BackendModel =
    { game : BGameStatus
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | DrawCardFromDeckFrontend
    | TamalouFrontend
    | DiscardCardFrontend
    | DrawCardFromDiscardPileFrontend
    | ReplaceCardInFrontend Int
    | DoubleCardFrontend Int



-- | CardShuffled Deck


type ToBackend
    = NoOpToBackend
    | TryToReconnectToBackend String
    | DrawCardFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawCardFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId SessionId ClientId
      -- | GotUserConnected SessionId ClientId
    | GotUserDisconnected SessionId ClientId
    | BeginGameAndDistribute4CardsToEach (List Card)
    | TimerTick Posix


type ToFrontend
    = NoOpToFrontend
    | ConnectedBack SessionId ClientId FGame
    | UpdateGame FGame
    | GotSessionIdAndClientId SessionId ClientId


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool
    | BGameEnded ClientId


type alias BPlayer =
    { name : String
    , tableHand : List Card
    , clientId : ClientId
    , sessionId : SessionId
    }


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded ClientId
    | FGameAlreadyStartedWithoutYou


type FGameInProgressStatus
    = FTimerRunning Int
    | FPlayerToPlay SessionId FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus


type FPlayerToPlayStatus
    = FWaitingPlayerDraw
    | FPlayerHasDraw FCard


type BGameInProgressStatus
    = BTimerRunning Int
    | BPlayerToPlay SessionId BPlayerToPlayStatus


type BPlayerToPlayStatus
    = BWaitingPlayerDraw
    | BPlayerHasDraw Card


type alias FPlayer =
    { name : String
    , tableHand : List FCard
    , clientId : ClientId
    , sessionId : SessionId
    }


type alias Deck =
    List Card


type GameId
    = GameId Int


type alias DiscardPile =
    List Card


type alias FDrawPile =
    List FCard


type alias FTableHand =
    List FCard


type alias BDrawPile =
    List Card
