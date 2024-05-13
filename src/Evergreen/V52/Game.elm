module Evergreen.V52.Game exposing (..)

import Evergreen.V52.Card
import Evergreen.V52.Counter
import Evergreen.V52.Player
import Lamdera
import Random


type alias TamalouOwner =
    { sessionId : Lamdera.SessionId
    , tableHand : List Evergreen.V52.Card.Card
    }


type alias FTableHand =
    List Evergreen.V52.Card.FCard


type alias FDrawPile =
    List Evergreen.V52.Card.FCard


type alias DiscardPile =
    List Evergreen.V52.Card.Card


type FGameInProgressStatus
    = FStartTimerRunning Evergreen.V52.Counter.Counter
    | FPlayerToPlay Evergreen.V52.Player.FPlayer Evergreen.V52.Player.FPlayerToPlayStatus
    | FYourTurn Evergreen.V52.Player.FPlayerToPlayStatus
    | FEndTimerRunning Evergreen.V52.Counter.Counter


type FGame
    = FWaitingForPlayers (List Evergreen.V52.Player.FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List Evergreen.V52.Player.FPlayer) FGameInProgressStatus
    | FGameEnded (List ( Evergreen.V52.Player.FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type alias BDrawPile =
    List Evergreen.V52.Card.Card


type BGameInProgressStatus
    = BStartTimerRunning Evergreen.V52.Counter.Counter
    | BPlayerToPlay Evergreen.V52.Player.BPlayer Evergreen.V52.Player.BPlayerToPlayStatus
    | BEndTimerRunning Evergreen.V52.Counter.Counter


type BGameStatus
    = BWaitingForPlayers (List Evergreen.V52.Player.BPlayer)
    | BGameInProgress (Maybe Lamdera.SessionId) BDrawPile DiscardPile (List Evergreen.V52.Player.BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( Evergreen.V52.Player.BPlayer, Int ))


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }
