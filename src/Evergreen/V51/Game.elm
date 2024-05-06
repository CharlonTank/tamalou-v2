module Evergreen.V51.Game exposing (..)

import Evergreen.V51.Card
import Evergreen.V51.Counter
import Evergreen.V51.Player
import Lamdera
import Random


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V51.Card.Card
    }


type alias FTableHand =
    List Evergreen.V51.Card.FCard


type alias FDrawPile =
    List Evergreen.V51.Card.FCard


type alias DiscardPile =
    List Evergreen.V51.Card.Card


type FGameInProgressStatus
    = FStartTimerRunning Evergreen.V51.Counter.Counter
    | FPlayerToPlay Evergreen.V51.Player.FPlayer Evergreen.V51.Player.FPlayerToPlayStatus
    | FYourTurn Evergreen.V51.Player.FPlayerToPlayStatus
    | FEndTimerRunning Evergreen.V51.Counter.Counter


type FGame
    = FWaitingForPlayers (List Evergreen.V51.Player.FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List Evergreen.V51.Player.FPlayer) FGameInProgressStatus
    | FGameEnded (List ( Evergreen.V51.Player.FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias BDrawPile =
    List Evergreen.V51.Card.Card


type BGameInProgressStatus
    = BStartTimerRunning Evergreen.V51.Counter.Counter
    | BPlayerToPlay Evergreen.V51.Player.BPlayer Evergreen.V51.Player.BPlayerToPlayStatus
    | BEndTimerRunning Evergreen.V51.Counter.Counter


type BGameStatus
    = BWaitingForPlayers (List Evergreen.V51.Player.BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List Evergreen.V51.Player.BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( Evergreen.V51.Player.BPlayer, Int ))


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }
