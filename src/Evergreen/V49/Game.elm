module Evergreen.V49.Game exposing (..)

import Evergreen.V49.Card
import Evergreen.V49.Counter
import Evergreen.V49.Player
import Lamdera
import Random


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V49.Card.Card
    }


type alias FTableHand =
    List Evergreen.V49.Card.FCard


type alias FDrawPile =
    List Evergreen.V49.Card.FCard


type alias DiscardPile =
    List Evergreen.V49.Card.Card


type FGameInProgressStatus
    = FStartTimerRunning Evergreen.V49.Counter.Counter
    | FPlayerToPlay Evergreen.V49.Player.FPlayer Evergreen.V49.Player.FPlayerToPlayStatus
    | FYourTurn Evergreen.V49.Player.FPlayerToPlayStatus
    | FEndTimerRunning Evergreen.V49.Counter.Counter


type FGame
    = FWaitingForPlayers (List Evergreen.V49.Player.FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List Evergreen.V49.Player.FPlayer) FGameInProgressStatus
    | FGameEnded (List ( Evergreen.V49.Player.FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias BDrawPile =
    List Evergreen.V49.Card.Card


type BGameInProgressStatus
    = BStartTimerRunning Evergreen.V49.Counter.Counter
    | BPlayerToPlay Evergreen.V49.Player.BPlayer Evergreen.V49.Player.BPlayerToPlayStatus
    | BEndTimerRunning Evergreen.V49.Counter.Counter


type BGameStatus
    = BWaitingForPlayers (List Evergreen.V49.Player.BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List Evergreen.V49.Player.BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( Evergreen.V49.Player.BPlayer, Int ))


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }
