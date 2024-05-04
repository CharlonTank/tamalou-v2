module Evergreen.V49.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V49.Animator.Timeline
import Evergreen.V49.Card
import Evergreen.V49.Game
import Evergreen.V49.Player
import Evergreen.V49.Positioning.Types
import Evergreen.V49.Utils.Ui
import Lamdera
import Time
import Url


type alias PositionedPlayer =
    { player : Evergreen.V49.Player.FPlayer
    , positionedTableHand : List ( Evergreen.V49.Card.FCard, Evergreen.V49.Animator.Timeline.Timeline Evergreen.V49.Positioning.Types.GBPosition )
    , namePosition : Evergreen.V49.Positioning.Types.GBPosition
    }


type alias OpponentsDisposition =
    { leftPlayer : Maybe PositionedPlayer
    , topLeftPlayer : Maybe PositionedPlayer
    , topRightPlayer : Maybe PositionedPlayer
    , rightPlayer : Maybe PositionedPlayer
    }


type alias Positions =
    { drawPilePosition : Evergreen.V49.Positioning.Types.GBPosition
    , cardsFromDrawPileMovingPositions : List (Evergreen.V49.Animator.Timeline.Timeline Evergreen.V49.Positioning.Types.GBPosition)
    , drewCardMovingPosition : Evergreen.V49.Animator.Timeline.Timeline Evergreen.V49.Positioning.Types.GBPosition
    , middleTextPosition : Evergreen.V49.Positioning.Types.GBPosition
    , discardPilePosition : Evergreen.V49.Positioning.Types.GBPosition
    , cardFromDiscardPileMovingPositions : Maybe (Evergreen.V49.Animator.Timeline.Timeline Evergreen.V49.Positioning.Types.GBPosition)
    , playAgainOrPassPosition : Evergreen.V49.Positioning.Types.GBPosition
    , opponentsDisposition : OpponentsDisposition
    , ownCardsDisposition : List ( Evergreen.V49.Card.FCard, Evergreen.V49.Animator.Timeline.Timeline Evergreen.V49.Positioning.Types.GBPosition )
    }


type GameDisposition
    = NotCalculated
    | Calculated Positions


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , device : Evergreen.V49.Utils.Ui.Device
    , fGame : Evergreen.V49.Game.FGame
    , clientId : Maybe Lamdera.ClientId
    , sessionId : Maybe Lamdera.SessionId
    , urlPath : String
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
    }


type alias BackendModel =
    { games : List Evergreen.V49.Game.BGame
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
    | AnimationReplaceCardInTableHand Lamdera.SessionId Int Evergreen.V49.Card.Card
    | AnimationDoubleCardSuccess Lamdera.SessionId Int Evergreen.V49.Card.Card
    | AnimationDoubleCardFailed Lamdera.SessionId Int Evergreen.V49.Card.Card
    | AnimationSwitchCards ( Lamdera.SessionId, Int ) ( Lamdera.SessionId, Int )
    | AnimationDiscardCard
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
    | ReStartGameFrontend (Maybe Evergreen.V49.Player.FPlayer)
    | TamalouFrontend
    | PowerIsUsedFrontend
    | PowerPassFrontend
    | ChangeChatInputFrontend String
    | SendMessageFrontend
    | CardClickMsg CardClickMsg
    | Frame Time.Posix
    | UpdateFGamePostAnimationFrontend Evergreen.V49.Game.FGame PlayerActionAnimation


type ActionFromGameToBackend
    = ConnectToBackend
    | ChangeCurrentPlayerNameToBackend String
    | ImReadyToBackend
    | ReStartGameToBackend (Maybe Evergreen.V49.Player.FPlayer)
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
    | UpdateGameStatusToFrontend Evergreen.V49.Game.FGame (Maybe PlayerActionAnimation)
    | UpdateGameAndChatToFrontend ( Evergreen.V49.Game.FGame, List ( String, String ) )
    | UpdateChatToFrontend (List ( String, String ))
    | GotSessionIdAndClientIdToFrontend Lamdera.SessionId Lamdera.ClientId
