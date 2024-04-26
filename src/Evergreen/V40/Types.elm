module Evergreen.V40.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V40.Animator.Timeline
import Evergreen.V40.Card
import Evergreen.V40.Ui
import Evergreen.V40.Utils.Ui
import Lamdera
import Random
import Time
import Url


type alias FPlayer =
    { name : String
    , tableHand : List Evergreen.V40.Card.FCard
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    , score : Maybe Int
    }


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V40.Card.Card
    }


type alias FTableHand =
    List Evergreen.V40.Card.FCard


type alias FDrawPile =
    List Evergreen.V40.Card.FCard


type alias DiscardPile =
    List Evergreen.V40.Card.Card


type Counter
    = Five
    | Four
    | Three
    | Two
    | One


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen
        Int
        { sessionId : Lamdera.SessionId
        , index : Int
        }
        Counter


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Evergreen.V40.Card.Power)
    | FPlayerHasDraw Evergreen.V40.Card.FCard
    | FPlayerHasDiscard Evergreen.V40.Card.Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus


type FGameInProgressStatus
    = FStartTimerRunning Counter
    | FPlayerToPlay FPlayer FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus
    | FEndTimerRunning Counter


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded (List ( FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V40.Ui.Angle
    }


type alias PositionedPlayer =
    { player : FPlayer
    , positionedTableHand : List ( Evergreen.V40.Card.FCard, Evergreen.V40.Animator.Timeline.Timeline GBPosition )
    , namePosition : GBPosition
    }


type alias OpponentsDisposition =
    { leftPlayer : Maybe PositionedPlayer
    , topLeftPlayer : Maybe PositionedPlayer
    , topRightPlayer : Maybe PositionedPlayer
    , rightPlayer : Maybe PositionedPlayer
    }


type alias Positions =
    { drawPilePosition : GBPosition
    , cardsFromDrawPileMovingPositions : List (Evergreen.V40.Animator.Timeline.Timeline GBPosition)
    , drewCardMovingPosition : Evergreen.V40.Animator.Timeline.Timeline GBPosition
    , middleTextPosition : GBPosition
    , discardPilePosition : GBPosition
    , cardFromDiscardPileMovingPositions : Maybe (Evergreen.V40.Animator.Timeline.Timeline GBPosition)
    , playAgainOrPassPosition : GBPosition
    , opponentsDisposition : OpponentsDisposition
    , ownCardsDisposition : List ( Evergreen.V40.Card.FCard, Evergreen.V40.Animator.Timeline.Timeline GBPosition )
    }


type GameDisposition
    = NotCalculated
    | Calculated Positions


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , device : Evergreen.V40.Utils.Ui.Device
    , fGame : FGame
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


type alias BPlayer =
    { name : String
    , tableHand : List Evergreen.V40.Card.Card
    , clientId : Lamdera.ClientId
    , sessionId : Lamdera.SessionId
    , ready : Bool
    }


type alias BDrawPile =
    List Evergreen.V40.Card.Card


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Evergreen.V40.Card.Power)
    | BPlayerHasDrawn Evergreen.V40.Card.Card
    | BPlayerHasDiscard Evergreen.V40.Card.Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus


type BGameInProgressStatus
    = BStartTimerRunning Counter
    | BPlayerToPlay BPlayer BPlayerToPlayStatus
    | BEndTimerRunning Counter


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( BPlayer, Int ))


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


type CardClickMsg
    = DrawCardFromDeckFrontend
    | DrawFromDiscardPileFrontend
    | DiscardCardFrontend
    | CardClickReplacement Int
    | DoubleCardFrontend Int
    | LookAtCardFrontend Int
    | ChooseOwnCardToSwitchFrontend Int
    | ChooseOpponentCardToSwitchFrontend Lamdera.SessionId Int


type PlayerAction
    = AnimationDrawCardFromDeck
    | AnimationDrawCardFromDiscardPile
    | AnimationReplaceCardInTableHand Lamdera.SessionId Int Evergreen.V40.Card.Card
    | AnimationDoubleCardSuccess Lamdera.SessionId Int Evergreen.V40.Card.Card
    | AnimationDoubleCardFailed Lamdera.SessionId Int Evergreen.V40.Card.Card
    | AnimationSwitchCards ( Lamdera.SessionId, Int ) ( Lamdera.SessionId, Int )
    | AnimationDiscardCard


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
    | ReStartGameFrontend (Maybe FPlayer)
    | TamalouFrontend
    | PowerIsUsedFrontend
    | PowerPassFrontend
    | ChangeChatInputFrontend String
    | SendMessageFrontend
    | CardClickMsg CardClickMsg
    | Frame Time.Posix
    | UpdateFGamePostAnimationFrontend FGame PlayerAction


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
    | UpdateGameStatusToFrontend FGame (Maybe PlayerAction)
    | UpdateGameAndChatToFrontend ( FGame, List ( String, String ) )
    | UpdateChatToFrontend (List ( String, String ))
    | GotSessionIdAndClientIdToFrontend Lamdera.SessionId Lamdera.ClientId
