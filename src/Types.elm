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
    }


type alias BackendModel =
    { game : BGame
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | DrawCardFromDeckFrontend
    | TamalouFrontend
    | DiscardCardFrontend
    | DrawCardFromDiscardPileFrontend



-- | CardShuffled Deck


type ToBackend
    = NoOpToBackend
    | DrawCardFromDrawPileToBackend
    | DiscardCardToBackend
    | DrawCardFromDiscardPileToBackend


type BackendMsg
    = NoOpBackendMsg
    | GotUserConnected SessionId ClientId
    | GotUserDisconnected SessionId ClientId
    | BeginGameAndDistribute4CardsToEach (List Card)
    | TimerTick Posix


type ToFrontend
    = NoOpToFrontend
    | ConnectedBack SessionId ClientId FGame
    | UpdateGame FGame


type BGame
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus
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


type FGameInProgressStatus
    = FTimerRunning Int
    | FPlayerToPlay SessionId FPlayerToPlayStatus


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
