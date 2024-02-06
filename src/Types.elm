module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Card exposing (Card)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , gameFrontend : FrontendGame
    , clientId : Maybe ClientId
    }


type alias BackendModel =
    { game : BackendGame
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg



-- | CardShuffled Deck


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg
    | GotUserConnected SessionId ClientId
    | GotUserDisconnected SessionId ClientId
    | BeginGameAndDistribute4CardsToEach (List Card)


type ToFrontend
    = NoOpToFrontend
    | ConnectedBack ClientId FrontendGame
    | UpdateGame FrontendGame


type BackendGame
    = BackendWaitingForPlayers (List BackendPlayer)
    | BackendGameInProgress DrawPile DiscardPile (List BackendPlayer)
    | BackendGameEnded ClientId


type alias BackendPlayer =
    { name : String
    , hand : List Card
    , clientId : ClientId
    , isConnected : Bool
    }


type FrontendGame
    = FrontendWaitingForPlayers (List FrontendPlayer)
    | FrontendGameInProgress DrawPile DiscardPile (List FrontendPlayer)
    | FrontendGameEnded ClientId


type alias FrontendPlayer =
    { name : String
    , hand : List Card
    , clientId : ClientId
    }


type alias Deck =
    List Card


type GameId
    = GameId Int


type alias DiscardPile =
    List Card


type alias DrawPile =
    List Card
