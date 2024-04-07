module Types exposing (ActionFromGameToBackend(..), BDrawPile, BGame, BGameInProgressStatus(..), BGameStatus(..), BPlayer, BPlayerToPlayStatus(..), BackendModel, BackendMsg(..), BackendMsgFromGame(..), CardClickMsg(..), Counter(..), DiscardPile, FDrawPile, FGame(..), FGameInProgressStatus(..), FPlayer, FPlayerToPlayStatus(..), FTableHand, FrontendModel, FrontendMsg(..), LookACardStatus(..), Switch2CardsStatus(..), TamalouOwner, ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Card exposing (Card, FCard, Power)
import Element exposing (Device)
import Lamdera exposing (ClientId, SessionId)
import Random
import Time exposing (Posix)
import Url exposing (Url)


type ActionFromGameToBackend
    = ConnectToBackend
    | ChangeCurrentPlayerNameToBackend String
    | ImReadyToBackend
    | ReStartGameToBackend (Maybe FPlayer)
    | DrawCardFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int
    | LookAtCardInTableHandToBackend Int
    | ChooseOwnCardToSwitchToBackend Int
    | ChooseOpponentCardToSwitchToBackend ( SessionId, Int )
    | PowerIsUsedToBackend
    | PowerIsNotUsedToBackend
    | TamalouToBackend
    | SendMessageToBackend String


type alias BDrawPile =
    List Card


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }


type BGameInProgressStatus
    = BStartTimerRunning Counter
    | BPlayerToPlay BPlayer BPlayerToPlayStatus
    | BEndTimerRunning Counter


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


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Power)
    | BPlayerHasDrawn Card
    | BPlayerHasDiscard Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus


type alias BackendModel =
    { games : List BGame
    , errors : List String
    , admins : List SessionId
    }


type BackendMsg
    = NoOpBackendMsg
    | FeedSessionIdAndClientId SessionId ClientId
    | GotUserDisconnected SessionId ClientId
    | BackendMsgFromGame String BackendMsgFromGame


type BackendMsgFromGame
    = TimerTick Posix
    | CreateGame Posix ClientId SessionId


type CardClickMsg
    = DrawCardFromDeckFrontend
    | DrawFromDiscardPileFrontend
    | DiscardCardFrontend
    | CardClickReplacement Int
    | DoubleCardFrontend Int
    | LookAtCardFrontend Int
    | ChooseOwnCardToSwitchFrontend Int
    | ChooseOpponentCardToSwitchFrontend SessionId Int


type Counter
    = Five
    | Four
    | Three
    | Two
    | One


type alias DiscardPile =
    List Card


type alias FDrawPile =
    List FCard


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded (List FPlayer)
    | FGameAlreadyStartedWithoutYou


type FGameInProgressStatus
    = FStartTimerRunning Counter
    | FPlayerToPlay FPlayer FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus
    | FEndTimerRunning Counter


type alias FPlayer =
    { name : String
    , tableHand : List FCard
    , clientId : ClientId
    , sessionId : SessionId
    , ready : Bool
    , score : Maybe Int
    }


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Power)
    | FPlayerHasDraw FCard
    | FPlayerHasDiscard Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus


type alias FTableHand =
    List FCard


type alias FrontendModel =
    { key : Key
    , device : Device
    , fGame : FGame
    , clientId : Maybe ClientId
    , sessionId : Maybe SessionId
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


type FrontendMsg
    = NoOpFrontendMsg
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotWindowSize { height : Int, width : Int }
    | ChangeCurrentPlayerNameFrontend String
    | ImReadyFrontend
    | ReStartGameFrontend (Maybe FPlayer)
    | TamalouFrontend
    | PowerIsUsedFrontend
    | PowerPassFrontend
    | ChangeChatInputFrontend String
    | SendMessageFrontend
    | CardClickMsg CardClickMsg


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen Int { sessionId : SessionId, index : Int } Counter


type alias TamalouOwner =
    { sessionId : SessionId
    , tableHand : List Card
    }


type ToBackend
    = NoOpToBackend
    | ActionFromGameToBackend String ActionFromGameToBackend
    | ConnectToAdminToBackend


type ToFrontend
    = NoOpToFrontend
    | UpdateAdminToFrontend (List String)
    | UpdateGameStatusToFrontend FGame
    | UpdateGameAndChatToFrontend ( FGame, List ( String, String ) )
    | UpdateChatToFrontend (List ( String, String ))
    | GotSessionIdAndClientIdToFrontend SessionId ClientId
