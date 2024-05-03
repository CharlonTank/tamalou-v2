module Types exposing (ActionFromGameToBackend(..), AddOrRemove(..), BDrawPile, BGame, BGameInProgressStatus(..), BGameStatus(..), BPlayer, BPlayerToPlayStatus(..), BackendModel, BackendMsg(..), BackendMsgFromGame(..), CardClickMsg(..), Counter(..), DiscardPile, FDrawPile, FGame(..), FGameInProgressStatus(..), FPlayer, FPlayerToPlayStatus(..), FTableHand, FrontendModel, FrontendMsg(..), GameDisposition(..), LookACardStatus(..), OpponentsDisposition, PlayerActionAnimation(..), PositionedPlayer, Positions, Switch2CardsStatus(..), TamalouOwner, ToBackend(..), ToFrontend(..))

import Animator.Timeline exposing (Timeline)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Card exposing (Card, FCard, Power)
import Lamdera exposing (ClientId, SessionId)
import Positioning.Types exposing (GBPosition)
import Random
import Time exposing (Posix)
import Url exposing (Url)
import Utils.Ui exposing (Device)


type ActionFromGameToBackend
    = ConnectToBackend
    | ChangeCurrentPlayerNameToBackend String
    | ImReadyToBackend
    | ReStartGameToBackend (Maybe FPlayer)
    | DrawFromDrawPileToBackend
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


type AddOrRemove
    = Add
    | Remove


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
    | BGameEnded (List ( BPlayer, Int ))


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
    | FGameEnded (List ( FPlayer, Int ))
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
    , viewPort : { height : Int, width : Int }
    , ready : Bool
    , maybeName : Maybe String
    , chatInput : String
    , chat : List ( String, String )
    , gameDisposition : GameDisposition

    -- , animationState : Ui.Anim.State
    , alreadyInAction : Bool
    , posix : Posix

    -- , animDur : Maybe Int
    -- , nextStates : List ( FGame, PlayerAction )
    -- , animations : List (Timeline GBPosition)
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
      -- | UpdateFlip CardAnimation
      -- | AnimMsg Ui.Anim.Msg
    | Frame Posix
    | UpdateFGamePostAnimationFrontend FGame PlayerActionAnimation


type GameDisposition
    = NotCalculated
    | Calculated Positions


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Counter


type alias OpponentsDisposition =
    { leftPlayer : Maybe PositionedPlayer
    , topLeftPlayer : Maybe PositionedPlayer
    , topRightPlayer : Maybe PositionedPlayer
    , rightPlayer : Maybe PositionedPlayer
    }


type PlayerActionAnimation
    = AnimationDrawCardFromDeck
    | AnimationDrawCardFromDiscardPile
    | AnimationReplaceCardInTableHand SessionId Int Card
    | AnimationDoubleCardSuccess SessionId Int Card
    | AnimationDoubleCardFailed SessionId Int Card
    | AnimationSwitchCards ( SessionId, Int ) ( SessionId, Int )
    | AnimationDiscardCard


type alias PositionedPlayer =
    { player : FPlayer
    , positionedTableHand : List ( FCard, Timeline GBPosition )
    , namePosition : GBPosition
    }


type alias Positions =
    { drawPilePosition : GBPosition
    , cardsFromDrawPileMovingPositions : List (Timeline GBPosition)
    , drewCardMovingPosition : Timeline GBPosition
    , middleTextPosition : GBPosition
    , discardPilePosition : GBPosition
    , cardFromDiscardPileMovingPositions : Maybe (Timeline GBPosition)
    , playAgainOrPassPosition : GBPosition
    , opponentsDisposition : OpponentsDisposition
    , ownCardsDisposition : List ( FCard, Timeline GBPosition )
    }


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
    | UpdateGameStatusToFrontend FGame (Maybe PlayerActionAnimation)
    | UpdateGameAndChatToFrontend ( FGame, List ( String, String ) )
    | UpdateChatToFrontend (List ( String, String ))
    | GotSessionIdAndClientIdToFrontend SessionId ClientId
