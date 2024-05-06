module Evergreen.V24.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V24.Card
import Lamdera
import Random
import Time
import Ui
import Url
import Utils.Ui as Ui


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V24.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V24.Card.Card
    }


type alias FTableHand =
    List Evergreen.V24.Card.FCard


type alias FDrawPile =
    List Evergreen.V24.Card.FCard


type alias DiscardPile =
    List Evergreen.V24.Card.Card


type Counter
    = Five
    | Four
    | Three
    | Two
    | One
    | Zero


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V24.Card.Power)
    | FPlayerHasDraw Evergreen.V24.Card.FCard
    | FPlayerHasDiscard Evergreen.V24.Card.Power
    | FPlayerLookACard (Maybe Counter)


type FGameInProgressStatus
    = FStartTimerRunning Counter
    | FPlayerToPlay Lamdera.SessionId FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus
    | FEndTimerRunning Counter


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded (List FPlayer)
    | FGameAlreadyStartedWithoutYou


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , device : Ui.Device
    , fGame : FGame
    , clientId : Maybe Lamdera.ClientId
    , sessionId : Maybe Lamdera.SessionId
    , urlPath : String
    , errors : List String
    , admin : Bool
    , screenHeight : Int
    , screenWidth : Int
    , ready : Bool
    , maybeName : Maybe String
    , chatInput : String
    , chat : List ( String, String )
    }


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V24.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type alias BDrawPile =
    List Evergreen.V24.Card.Card


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V24.Card.Power)
    | BPlayerHasDraw Evergreen.V24.Card.Card
    | BPlayerHasDiscard Evergreen.V24.Card.Power
    | BPlayerLookACard (Maybe Counter)


type BGameInProgressStatus
    = BStartTimerRunning Counter
    | BPlayerToPlay Lamdera.SessionId BPlayerToPlayStatus
    | BEndTimerRunning Counter


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List BPlayer)


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }


type alias BackendModel =
    { games : List BGame
    , errors : List String
    , admins : List Lamdera.SessionId
    }


type FrontendMsg
    = NoOpFrontendMsg
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotWindowSize
        { height : Int
        , width : Int
        }
    | ChangeCurrentPlayerNameFrontend String
    | ImReadyFrontend
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
    | ChangeChatInputFrontend String
    | SendMessageFrontend


type ActionFromGameToBackend
    = ConnectToBackend
    | ChangeCurrentPlayerNameToBackend String
    | ImReadyToBackend
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
    | SendMessageToBackend String


type ToBackend
    = NoOpToBackend
    | ActionFromGameToBackend String ActionFromGameToBackend
    | ConnectToAdminToBackend


type BackendMsgFromGame
    = TimerTick Time.Posix
    | CreateGame Time.Posix Lamdera.ClientId Lamdera.SessionId


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId Lamdera.SessionId Lamdera.ClientId
    | GotUserDisconnected Lamdera.SessionId Lamdera.ClientId
    | BackendMsgFromGame String BackendMsgFromGame


type ToFrontend
    = NoOpToFrontend
    | UpdateAdminToFrontend (List String)
    | UpdateGameStatusToFrontend FGame
    | UpdateGameAndChatToFrontend ( FGame, List ( String, String ) )
    | UpdateChatToFrontend (List ( String, String ))
    | GotSessionIdAndClientIdToFrontend Lamdera.SessionId Lamdera.ClientId
