module Evergreen.V55.Game exposing (..)

import Evergreen.V55.Card
import Evergreen.V55.Counter
import Evergreen.V55.Player
import Lamdera
import Random


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V55.Card.Card
    }


type alias FTableHand =
    List Evergreen.V55.Card.FCard


type alias FDrawPile =
    List Evergreen.V55.Card.FCard


type alias DiscardPile =
    List Evergreen.V55.Card.Card


type FGameInProgressStatus
    = FStartTimerRunning Evergreen.V55.Counter.Counter
    | FPlayerToPlay Evergreen.V55.Player.CurrentPlayer Evergreen.V55.Player.FPlayerToPlayStatus
    | FYourTurn Evergreen.V55.Player.FPlayerToPlayStatus
    | FEndTimerRunning Evergreen.V55.Counter.Counter


type FGame
    = FWaitingForPlayers (List Evergreen.V55.Player.FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List Evergreen.V55.Player.FPlayer) FGameInProgressStatus
    | FGameEnded (List ( Evergreen.V55.Player.FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias BDrawPile =
    List Evergreen.V55.Card.Card


type BGameInProgressStatus
    = BStartTimerRunning Evergreen.V55.Counter.Counter
    | BPlayerToPlay Evergreen.V55.Player.CurrentPlayer Evergreen.V55.Player.BPlayerToPlayStatus
    | BEndTimerRunning Evergreen.V55.Counter.Counter


type BGameStatus
    = BWaitingForPlayers (List Evergreen.V55.Player.BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List Evergreen.V55.Player.BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( Evergreen.V55.Player.BPlayer, Int ))


type alias BGame =
    { name : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }
