module Evergreen.V55.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V55.Animator.Timeline
import Evergreen.V55.Card
import Evergreen.V55.Game
import Evergreen.V55.Player
import Evergreen.V55.Positioning.Types
import Evergreen.V55.Router
import Evergreen.V55.Utils.Ui
import Lamdera
import Time
import Url


type alias PositionedPlayer =
    { player : Evergreen.V55.Player.FPlayer
    , positionedTableHand : List ( Evergreen.V55.Card.FCard, Evergreen.V55.Animator.Timeline.Timeline Evergreen.V55.Positioning.Types.GBPosition )
    , namePosition : Evergreen.V55.Positioning.Types.GBPosition
    }


type alias OpponentsDisposition =
    { leftPlayer : Maybe PositionedPlayer
    , topLeftPlayer : Maybe PositionedPlayer
    , topRightPlayer : Maybe PositionedPlayer
    , rightPlayer : Maybe PositionedPlayer
    }


type alias Positions =
    { drawPilePosition : Evergreen.V55.Positioning.Types.GBPosition
    , cardsFromDrawPileMovingPositions : List (Evergreen.V55.Animator.Timeline.Timeline Evergreen.V55.Positioning.Types.GBPosition)
    , drewCardMovingPosition : Maybe (Evergreen.V55.Animator.Timeline.Timeline Evergreen.V55.Positioning.Types.GBPosition)
    , drewCardPosition : Evergreen.V55.Animator.Timeline.Timeline Evergreen.V55.Positioning.Types.GBPosition
    , middleTextPosition : Evergreen.V55.Positioning.Types.GBPosition
    , discardPilePosition : Evergreen.V55.Positioning.Types.GBPosition
    , cardFromDiscardPileMovingPosition : Maybe (Evergreen.V55.Animator.Timeline.Timeline Evergreen.V55.Positioning.Types.GBPosition)
    , playAgainOrPassPosition : Evergreen.V55.Positioning.Types.GBPosition
    , opponentsDisposition : OpponentsDisposition
    , ownCardsDisposition : List ( Evergreen.V55.Card.FCard, Evergreen.V55.Animator.Timeline.Timeline Evergreen.V55.Positioning.Types.GBPosition )
    }


type GameDisposition
    = NotCalculated
    | Calculated Positions


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , device : Evergreen.V55.Utils.Ui.Device
    , fGame : Maybe Evergreen.V55.Game.FGame
    , roomName : String
    , clientId : Maybe Lamdera.ClientId
    , sessionId : Maybe Lamdera.SessionId
    , errors : List String
    , admin : Bool
    , viewPort :
        { height : Int
        , width : Int
        }
    , ready : Bool
    , maybeName : Maybe String
    , chatInput : String
    , chat : List ( String, String )
    , gameDisposition : GameDisposition
    , alreadyInAction : Bool
    , posix : Time.Posix
    , route : Evergreen.V55.Router.Route
    }


type alias BackendModel =
    { games : List Evergreen.V55.Game.BGame
    , errors : List String
    , admins : List Lamdera.SessionId
    }


type CardClickMsg
    = DrawCardFromDeckFrontend
    | DrawFromDiscardPileFrontend
    | DiscardCardFrontend
    | CardClickReplacement Int
    | DoubleCardFrontend Int
    | LookAtCardFrontend Int
    | ChooseOwnCardToSwitchFrontend Int
    | ChooseOpponentCardToSwitchFrontend Lamdera.SessionId Int


type PlayerActionAnimation
    = AnimationDrawCardFromDeck
    | AnimationDrawCardFromDiscardPile
    | AnimationReplaceCardInTableHand Lamdera.SessionId Int Evergreen.V55.Card.Card
    | AnimationDoubleCardSuccess Lamdera.SessionId Int Evergreen.V55.Card.Card
    | AnimationDoubleCardFailed Lamdera.SessionId Int Evergreen.V55.Card.Card
    | AnimationSwitchCards ( Lamdera.SessionId, Int ) ( Lamdera.SessionId, Int )
    | AnimationDiscardCard
    | AnimationTamalouFailed Lamdera.SessionId
    | NoPlayerAction


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
    | ReStartGameFrontend (Maybe Evergreen.V55.Player.FPlayer)
    | TamalouFrontend
    | PowerIsUsedFrontend
    | PowerPassFrontend
    | ChangeChatInputFrontend String
    | SendMessageFrontend
    | CardClickMsg CardClickMsg
    | Frame Time.Posix
    | UpdateFGamePostAnimationFrontend Evergreen.V55.Game.FGame PlayerActionAnimation
    | ChangeRoomNameFrontend String
    | JoinRoomGameFrontend String


type ActionFromGameToBackend
    = ConnectToBackend
    | ChangeCurrentPlayerNameToBackend String
    | ImReadyToBackend
    | ReStartGameToBackend (Maybe Evergreen.V55.Player.FPlayer)
    | DrawFromDrawPileToBackend
    | DiscardCardInHandToBackend
    | DrawFromDiscardPileToBackend
    | ReplaceCardInTableHandToBackend Int
    | DoubleCardInTableHandToBackend Int
    | LookAtCardInTableHandToBackend Int
    | ChooseOwnCardToSwitchToBackend Int
    | ChooseOpponentCardToSwitchToBackend ( Lamdera.SessionId, Int )
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
    | UpdateGameStatusToFrontend Evergreen.V55.Game.FGame (Maybe PlayerActionAnimation)
    | UpdateGameAndChatToFrontend ( Evergreen.V55.Game.FGame, List ( String, String ) )
    | UpdateChatToFrontend (List ( String, String ))
    | GotSessionIdAndClientIdToFrontend Lamdera.SessionId Lamdera.ClientId
