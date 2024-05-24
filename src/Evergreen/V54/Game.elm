module Evergreen.V54.Game exposing (..)

import Evergreen.V54.Card
import Evergreen.V54.Counter
import Evergreen.V54.Player
import Lamdera
import Random


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V54.Card.Card
    }


type alias FTableHand =
    List Evergreen.V54.Card.FCard


type alias FDrawPile =
    List Evergreen.V54.Card.FCard


type alias DiscardPile =
    List Evergreen.V54.Card.Card


type FGameInProgressStatus
    = FStartTimerRunning Evergreen.V54.Counter.Counter
    | FPlayerToPlay Evergreen.V54.Player.FPlayer Evergreen.V54.Player.FPlayerToPlayStatus
    | FYourTurn Evergreen.V54.Player.FPlayerToPlayStatus
    | FEndTimerRunning Evergreen.V54.Counter.Counter


type FGame
    = FWaitingForPlayers (List Evergreen.V54.Player.FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List Evergreen.V54.Player.FPlayer) FGameInProgressStatus
    | FGameEnded (List ( Evergreen.V54.Player.FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias BDrawPile =
    List Evergreen.V54.Card.Card


type BGameInProgressStatus
    = BStartTimerRunning Evergreen.V54.Counter.Counter
    | BPlayerToPlay Evergreen.V54.Player.BPlayer Evergreen.V54.Player.BPlayerToPlayStatus
    | BEndTimerRunning Evergreen.V54.Counter.Counter


type BGameStatus
    = BWaitingForPlayers (List Evergreen.V54.Player.BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List Evergreen.V54.Player.BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( Evergreen.V54.Player.BPlayer, Int ))


type alias BGame =
    { name : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }
