module Evergreen.V53.Game exposing (..)

import Evergreen.V53.Card
import Evergreen.V53.Counter
import Evergreen.V53.Player
import Lamdera
import Random


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V53.Card.Card
    }


type alias FTableHand =
    List Evergreen.V53.Card.FCard


type alias FDrawPile =
    List Evergreen.V53.Card.FCard


type alias DiscardPile =
    List Evergreen.V53.Card.Card


type FGameInProgressStatus
    = FStartTimerRunning Evergreen.V53.Counter.Counter
    | FPlayerToPlay Evergreen.V53.Player.FPlayer Evergreen.V53.Player.FPlayerToPlayStatus
    | FYourTurn Evergreen.V53.Player.FPlayerToPlayStatus
    | FEndTimerRunning Evergreen.V53.Counter.Counter


type FGame
    = FWaitingForPlayers (List Evergreen.V53.Player.FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List Evergreen.V53.Player.FPlayer) FGameInProgressStatus
    | FGameEnded (List ( Evergreen.V53.Player.FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias BDrawPile =
    List Evergreen.V53.Card.Card


type BGameInProgressStatus
    = BStartTimerRunning Evergreen.V53.Counter.Counter
    | BPlayerToPlay Evergreen.V53.Player.BPlayer Evergreen.V53.Player.BPlayerToPlayStatus
    | BEndTimerRunning Evergreen.V53.Counter.Counter


type BGameStatus
    = BWaitingForPlayers (List Evergreen.V53.Player.BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List Evergreen.V53.Player.BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( Evergreen.V53.Player.BPlayer, Int ))


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }
