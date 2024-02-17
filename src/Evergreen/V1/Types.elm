module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Card
import Lamdera
import Url


type alias FrontendPlayer =
    { name : String
    , hand : List Evergreen.V1.Card.Card
    , clientId : Lamdera.ClientId
    }


type alias DrawPile =
    List Evergreen.V1.Card.Card


type alias DiscardPile =
    List Evergreen.V1.Card.Card


type FrontendGame
    = FrontendWaitingForPlayers (List FrontendPlayer)
    | FrontendGameInProgress DrawPile DiscardPile (List FrontendPlayer)
    | FrontendGameEnded Lamdera.ClientId


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , gameFrontend : FrontendGame
    , clientId : Maybe Lamdera.ClientId
    }


type alias BackendPlayer =
    { name : String
    , hand : List Evergreen.V1.Card.Card
    , clientId : Lamdera.ClientId
    , age : Int
    }


type BackendGame
    = BackendWaitingForPlayers (List BackendPlayer)
    | BackendGameInProgress DrawPile DiscardPile (List BackendPlayer)
    | BackendGameEnded Lamdera.ClientId


type alias BackendModel =
    { game : BackendGame
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg
    | GotUserConnected Lamdera.SessionId Lamdera.ClientId
    | GotUserDisconnected Lamdera.SessionId Lamdera.ClientId
    | BeginGameAndDistribute4CardsToEach (List Evergreen.V1.Card.Card)


type ToFrontend
    = NoOpToFrontend
    | ConnectedBack Lamdera.ClientId FrontendGame
    | UpdateGame FrontendGame
