module Debuggy.Logs exposing (bGameInProgressLogs)

import Card
import Counter exposing (Counter(..))
import Game exposing (BGameInProgressStatus(..), BGameStatus(..))
import Player exposing (BPlayer, BPlayerToPlayStatus(..), CurrentPlayer, LookACardStatus(..), Switch2CardsStatus(..))
import Utils.String as String


bGameInProgressLogs : String -> BGameStatus -> String
bGameInProgressLogs msg status =
    case status of
        BWaitingForPlayers players ->
            msg ++ ": BWaitingForPlayers: " ++ (List.map bPlayerLogs players |> String.join ", ")

        BGameInProgress _ _ _ players gameInProgressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
            msg ++ "BGameInProgress: " ++ (List.map bPlayerLogs players |> String.join ", ") ++ ", gameInProgressStatus: " ++ bGameInProgressStatusLogs gameInProgressStatus ++ ", lastMoveIsDouble: " ++ String.fromBool lastMoveIsDouble ++ " canUsePowerFromLastPlayer: " ++ String.fromBool canUsePowerFromLastPlayer

        BGameEnded players ->
            msg ++ "BGameEnded: " ++ (List.map bPlayerLogs (List.map Tuple.first players) |> String.join ", ")


bGameInProgressStatusLogs : BGameInProgressStatus -> String
bGameInProgressStatusLogs status =
    case status of
        BStartTimerRunning counter ->
            "BStartTimerRunning: " ++ counterToString counter

        BPlayerToPlay player playerToPlayStatus ->
            "BPlayerToPlay: " ++ currentPlayerLogs player ++ ", playerToPlayStatus: " ++ bPlayerToPlayStatusLogs playerToPlayStatus

        BEndTimerRunning counter ->
            "BEndTimerRunning: " ++ counterToString counter


currentPlayerLogs : CurrentPlayer -> String
currentPlayerLogs player =
    "name: " ++ player.name ++ " clientId: " ++ player.clientId ++ " sessionId: " ++ player.sessionId


bPlayerLogs : BPlayer -> String
bPlayerLogs player =
    "name: " ++ player.name ++ " clientId: " ++ player.clientId ++ " sessionId: " ++ player.sessionId ++ " ready: " ++ String.fromBool player.ready


bPlayerToPlayStatusLogs : BPlayerToPlayStatus -> String
bPlayerToPlayStatusLogs status =
    case status of
        BWaitingPlayerAction maybePower ->
            "BWaitingPlayerAction: "
                ++ (case maybePower of
                        Just power ->
                            "Just " ++ Card.powerToString power

                        Nothing ->
                            "Nothing"
                   )

        BPlayerHasDrawn card ->
            "BPlayerHasDraw: " ++ Card.toString card

        BPlayerHasDiscard power ->
            "BPlayerHasDiscard: " ++ Card.powerToString power

        BPlayerLookACard lookACardStatus ->
            "BPlayerLookACard: " ++ lookACardStatusLogs lookACardStatus

        BPlayerSwitch2Cards switch2CardsStatus ->
            "BPlayerSwitch2Cards: " ++ switch2CardsStatusLogs switch2CardsStatus

        BPlayerDisplayTamalouFailure cards counter ->
            "BPlayerDisplayTamalouFailure: " ++ (List.map Card.toString cards |> String.join ", ") ++ ", " ++ counterToString counter


counterToString : Counter -> String
counterToString counter =
    case counter of
        Five ->
            "Five"

        Four ->
            "Four"

        Three ->
            "Three"

        Two ->
            "Two"

        One ->
            "One"


lookACardStatusLogs : LookACardStatus -> String
lookACardStatusLogs status =
    case status of
        ChooseCardToLook ->
            "ChooseCardToLook"

        LookingACard index counter ->
            "LookingACard: " ++ String.fromInt index ++ ", " ++ counterToString counter


switch2CardsStatusLogs : Switch2CardsStatus -> String
switch2CardsStatusLogs status =
    case status of
        ChooseOwnCardToSwitch ->
            "ChooseOwnCardToSwitch"

        OwnCardChosen index ->
            "OwnCardChosen: " ++ String.fromInt index

        OpponentCardChosen ownCardIndex opponentCardChoosen counter ->
            "OpponentCardChosen: ownCardIndex: " ++ String.fromInt ownCardIndex ++ ", opponentCardChoosen: { sessionId: " ++ opponentCardChoosen.sessionId ++ ", index: " ++ String.fromInt opponentCardChoosen.index ++ " }, " ++ counterToString counter
