module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Card exposing (Card, FCard, Power)
import Element exposing (Device)
import Lamdera exposing (ClientId, SessionId)
import Random
import Time exposing (Posix)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , device : Device
    , gameFrontend : FGame
    , clientId : Maybe ClientId
    , sessionId : Maybe SessionId
    , urlPath : String
    , errors : List String
    , admin : Bool
    , screenHeight : Int
    , screenWidth : Int
    , ready : Bool
    }


type alias BackendModel =
    { games : List BGame
    , errors : List String
    , admins : List SessionId
    }


type FrontendMsg
    = NoOpFrontendMsg
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotWindowSize { height : Int, width : Int }
    | ChangeCurrentPlayerNameFrontend String
    | StartGameFrontend
    | ReStartGameFrontend
    | DrawCardFromDeckFrontend
    | TamalouFrontend
    | DiscardCardFrontend
    | DrawFromDiscardPileFrontend
    | PowerIsUsedFrontend
    | PowerPassFrontend
    | ReplaceCardInFrontend Int
    | DoubleCardFrontend Int
    | LookAtCardFrontend Int


type ToBackend
    = NoOpToBackend
    | ActionFromGameToBackend String ActionFromGameToBackend
    | ConnectToAdminToBackend


type ActionFromGameToBackend
    = ConnectToBackend
    | ChangeCurrentPlayerNameToBackend String
    | StartGameToBackend
    | ReStartGameToBackend
    | DrawCardFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int
    | LookAtCardInTableHandToBackend Int
    | PowerIsUsedToBackend
    | PowerIsNotUsedToBackend
    | TamalouToBackend


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId SessionId ClientId
    | GotUserDisconnected SessionId ClientId
    | BackendMsgFromGame String BackendMsgFromGame


type BackendMsgFromGame
    = TimerTick Posix
    | CreateGame Posix ClientId SessionId


type ToFrontend
    = NoOpToFrontend
    | UpdateAdminToFrontend (List String)
    | UpdateGameToFrontend FGame
    | GotSessionIdAndClientIdToFrontend SessionId ClientId


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , seed : Random.Seed
    }


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress (Maybe SessionId) BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List BPlayer)


type alias BPlayer =
    { name : String
    , tableHand : List Card
    , clientId : ClientId
    , sessionId : SessionId
    , ready : Bool
    }


type alias TamalouOwner =
    { sessionId : SessionId
    , tableHand : List Card
    }


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded (List FPlayer)
    | FGameAlreadyStartedWithoutYou


type FGameInProgressStatus
    = FStartTimerRunning Counter
    | FPlayerToPlay SessionId FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus
    | FEndTimerRunning Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Power)
    | FPlayerHasDraw FCard
    | FPlayerHasDiscard Power
    | FPlayerLookACard (Maybe Counter)


type BGameInProgressStatus
    = BStartTimerRunning Counter
    | BPlayerToPlay SessionId BPlayerToPlayStatus
    | BEndTimerRunning Counter


type Counter
    = Five
    | Four
    | Three
    | Two
    | One
    | Zero


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Power)
    | BPlayerHasDraw Card
    | BPlayerHasDiscard Power
    | BPlayerLookACard (Maybe Counter)


type alias FPlayer =
    { name : String
    , tableHand : List FCard
    , clientId : ClientId
    , sessionId : SessionId
    , ready : Bool
    , score : Maybe Int
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
