module Types exposing (ActionFromGameToBackend(..), AddOrRemove(..), BackendModel, BackendMsg(..), BackendMsgFromGame(..), CardClickMsg(..), FrontendModel, FrontendMsg(..), GameDisposition(..), OpponentsDisposition, PlayerActionAnimation(..), PositionedPlayer, Positions, ToBackend(..), ToFrontend(..))

import Animator.Timeline exposing (Timeline)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Card exposing (Card, FCard)
import Game exposing (..)
import Lamdera exposing (ClientId, SessionId)
import Player exposing (FPlayer)
import Positioning.Types exposing (GBPosition)
import Router exposing (Route)
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


type alias FrontendModel =
    { key : Key
    , device : Device
    , fGame : Maybe FGame
    , roomName : String
    , clientId : Maybe ClientId
    , sessionId : Maybe SessionId

    -- , urlPath : String
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
    , route : Route
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
    | ChangeRoomNameFrontend String
    | JoinRoomGameFrontend String


type GameDisposition
    = NotCalculated
    | Calculated Positions


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
    | AnimationTamalouFailed SessionId
    | NoPlayerAction


type alias PositionedPlayer =
    { player : FPlayer
    , positionedTableHand : List ( FCard, Timeline GBPosition )
    , namePosition : GBPosition
    }


type alias Positions =
    { drawPilePosition : GBPosition
    , cardsFromDrawPileMovingPositions : List (Timeline GBPosition)
    , drewCardMovingPosition : Maybe (Timeline GBPosition)
    , drewCardPosition : Timeline GBPosition
    , middleTextPosition : GBPosition
    , discardPilePosition : GBPosition
    , cardFromDiscardPileMovingPosition : Maybe (Timeline GBPosition)
    , playAgainOrPassPosition : GBPosition
    , opponentsDisposition : OpponentsDisposition
    , ownCardsDisposition : List ( FCard, Timeline GBPosition )
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
