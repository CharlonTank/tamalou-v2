module GameActionsToBackend exposing (..)

import Card exposing (Card, handIsLessThanFive)
import Counter exposing (Counter(..))
import Debuggy.Logs
import Game exposing (..)
import GameLogics exposing (..)
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Player exposing (..)
import Random
import Types exposing (..)
import Utils.Random exposing (drawCardFromDrawPile, generateRandomFunnyName, shuffleWithSeed)


handleActionFromGameToBackend : BackendModel -> String -> SessionId -> ClientId -> BGame -> ActionFromGameToBackend -> ( BackendModel, Cmd BackendMsg )
handleActionFromGameToBackend ({ games, errors } as model) urlPath sessionId clientId game toBackendActionFromGame =
    case toBackendActionFromGame of
        ConnectToBackend ->
            case game.status of
                BWaitingForPlayers players ->
                    let
                        frontendGame : FGame
                        frontendGame =
                            toFGame Nothing newGameStatus

                        ( funnyName, newSeed ) =
                            generateRandomFunnyName game.seed (List.map .name players)

                        newGameStatus : BGameStatus
                        newGameStatus =
                            BWaitingForPlayers newPlayers

                        newPlayers : List BPlayer
                        newPlayers =
                            case List.Extra.find ((==) sessionId << .sessionId) players of
                                Just _ ->
                                    players

                                Nothing ->
                                    { name = funnyName
                                    , tableHand = []
                                    , clientId = clientId
                                    , sessionId = sessionId
                                    , ready = False
                                    }
                                        :: players
                    in
                    ( { model | games = updateGameStatus urlPath ( BWaitingForPlayers newPlayers, newSeed ) games }
                    , Cmd.batch <|
                        Lamdera.sendToFrontend clientId (UpdateChatToFrontend game.chat)
                            :: List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame Nothing)) newPlayers
                    )

                BGameInProgress maybeTamalouOwner drawPile discardPile players progressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
                    case List.Extra.find ((==) sessionId << .sessionId) players of
                        Just _ ->
                            let
                                newGameStatus : BGameStatus
                                newGameStatus =
                                    BGameInProgress maybeTamalouOwner drawPile discardPile updateClientIdInPlayers progressStatus lastMoveIsDouble canUsePowerFromLastPlayer

                                updateClientIdInPlayers : List BPlayer
                                updateClientIdInPlayers =
                                    List.map
                                        (\p ->
                                            if p.sessionId == sessionId then
                                                { p | clientId = clientId }

                                            else
                                                p
                                        )
                                        players
                            in
                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                            , Cmd.batch <|
                                List.map (\p -> Lamdera.sendToFrontend p.clientId <| UpdateGameStatusToFrontend (toFGame (Just p.sessionId) newGameStatus) Nothing) updateClientIdInPlayers
                            )

                        Nothing ->
                            ( model, Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend FGameAlreadyStartedWithoutYou Nothing) )

                BGameEnded players ->
                    ( model
                    , Lamdera.sendToFrontend clientId (UpdateGameAndChatToFrontend ( toFGame Nothing (BGameEnded players), game.chat ))
                    )

        ChangeCurrentPlayerNameToBackend newName ->
            case game.status of
                BWaitingForPlayers players ->
                    let
                        frontendGame : FGame
                        frontendGame =
                            toFGame Nothing newGameStatus

                        newGameStatus : BGameStatus
                        newGameStatus =
                            BWaitingForPlayers newPlayers

                        newPlayers : List BPlayer
                        newPlayers =
                            List.map
                                (\p ->
                                    if p.sessionId == sessionId then
                                        { p | name = newName }

                                    else
                                        p
                                )
                                players
                    in
                    ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame Nothing)) newPlayers
                    )

                _ ->
                    ( model, Cmd.none )

        ImReadyToBackend ->
            case game.status of
                BWaitingForPlayers players ->
                    let
                        allPlayersReady : Bool
                        allPlayersReady =
                            List.all .ready newPlayers

                        newPlayers : List BPlayer
                        newPlayers =
                            List.map
                                (\p ->
                                    if p.sessionId == sessionId then
                                        { p | ready = True }

                                    else
                                        p
                                )
                                players
                    in
                    if allPlayersReady && List.length newPlayers >= 2 then
                        let
                            frontendGame : FGame
                            frontendGame =
                                toFGame Nothing newGameStatus

                            ( newDrawPile, newSeed ) =
                                -- shuffleWithSeed game.seed (Debug.log "WARNING" Debuggy.Decks.queens)
                                shuffleWithSeed game.seed Card.nonShuffledDeck

                            ( newDrawPile_, newPlayers_ ) =
                                List.foldl
                                    (\player ( drawPile_, players_ ) ->
                                        let
                                            ( newDrawPile__, newPlayer ) =
                                                distribute4CardsToPlayer drawPile_ player
                                        in
                                        ( newDrawPile__, players_ ++ [ newPlayer ] )
                                    )
                                    ( newDrawPile, [] )
                                    newPlayers

                            newGameStatus : BGameStatus
                            newGameStatus =
                                BGameInProgress Nothing newDrawPile_ [] newPlayers_ (BStartTimerRunning Five) False False
                        in
                        ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame Nothing)) newPlayers
                        )

                    else
                        let
                            frontendGame : FGame
                            frontendGame =
                                toFGame Nothing newGameStatus

                            newChat : List ( String, String )
                            newChat =
                                game.chat ++ [ ( Maybe.withDefault "" (List.Extra.find ((==) sessionId << .sessionId) newPlayers |> Maybe.map .name), "Let's go I'm ready!" ) ]

                            newGame : BGame
                            newGame =
                                { game | status = newGameStatus, chat = newChat }

                            newGameStatus : BGameStatus
                            newGameStatus =
                                BWaitingForPlayers newPlayers
                        in
                        ( { model | games = updateGame newGame games }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameAndChatToFrontend ( frontendGame, newChat ))) newPlayers
                        )

                _ ->
                    ( model, Cmd.none )

        ReStartGameToBackend maybeFPlayer ->
            case game.status of
                BWaitingForPlayers players ->
                    let
                        frontendGame : FGame
                        frontendGame =
                            toFGame Nothing newGameStatus

                        newGameStatus : BGameStatus
                        newGameStatus =
                            BWaitingForPlayers newPlayers

                        newPlayers : List BPlayer
                        newPlayers =
                            players ++ [ maybeFPlayer |> Maybe.map toBPlayer |> Maybe.map (\p -> { p | ready = False }) |> Maybe.withDefault (emptyBPlayer sessionId clientId) ]
                    in
                    ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame Nothing)) newPlayers
                    )

                BGameEnded players ->
                    let
                        frontendGame : FGame
                        frontendGame =
                            toFGame Nothing newGameStatus

                        newGameStatus : BGameStatus
                        newGameStatus =
                            BWaitingForPlayers newPlayers

                        newPlayers : List BPlayer
                        newPlayers =
                            players
                                -- Warning, here we remove all the other players in case they disconnect wihtout clicking restart, In the future, we want to send the score instead of the players so that we can remove them from the game on disconnect
                                |> List.map Tuple.first
                                |> List.filter ((==) sessionId << .sessionId)
                                |> List.map
                                    (\p ->
                                        { p
                                            | tableHand = []
                                            , ready = False
                                        }
                                    )
                    in
                    ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                    , Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend frontendGame Nothing)
                    )

                _ ->
                    ( model, Cmd.none )

        DrawFromDrawPileToBackend ->
            case game.status of
                BGameInProgress maybeTamalouOwner _ _ players (BPlayerToPlay bPlayer (BWaitingPlayerAction _)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                    if sessionId == bPlayer.sessionId then
                        case drawCardFromDrawPile game of
                            ( Just cardDrew, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                let
                                    newGameStatus : BGameStatus
                                    newGameStatus =
                                        BGameInProgress maybeTamalouOwner newDrawPile newDiscardPile players (BPlayerToPlay bPlayer (BPlayerHasDrawn cardDrew)) lastMoveIsDouble canUsePowerFromLastPlayer
                                in
                                updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, newSeed ) players (Just AnimationDrawCardFromDeck)

                            ( Nothing, _ ) ->
                                -- TODO: Terminate game because this should not happen, we need to log this...
                                let
                                    newGameStatus : BGameStatus
                                    newGameStatus =
                                        BGameEnded (assignRanks maybeTamalouOwner (stopDisplayCards maybeTamalouOwner players))
                                in
                                updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DiscardCardInHandToBackend ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerHasDrawn card)) _ _ ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                case Card.toPower (maybeTamalouOwner == Nothing || List.length players > 2) card of
                                    Just powerCard ->
                                        BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BPlayerToPlay bPlayer (BPlayerHasDiscard powerCard)) False False

                                    Nothing ->
                                        case nextPlayer maybeTamalouOwner sessionId players of
                                            Just nextPlayer_ ->
                                                BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BPlayerToPlay nextPlayer_ (BWaitingPlayerAction Nothing)) False False

                                            Nothing ->
                                                BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BEndTimerRunning Five) False False
                        in
                        ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) (Just AnimationDiscardCard)) players
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DrawFromDiscardPileToBackend ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile (head :: rest) players (BPlayerToPlay bPlayer (BWaitingPlayerAction _)) _ _ ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                BGameInProgress maybeTamalouOwner drawPile rest players (BPlayerToPlay bPlayer (BPlayerHasDrawn head)) False False
                        in
                        ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) (Just AnimationDrawCardFromDiscardPile)) players
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReplaceCardInTableHandToBackend cardIndex ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerHasDrawn cardInHand)) _ _ ->
                    if sessionId == bPlayer.sessionId then
                        let
                            maybeCardToDiscard : Maybe Card
                            maybeCardToDiscard =
                                maybeCurrentPlayer
                                    |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)
                                    |> Maybe.map (\card -> { card | show = True })

                            maybeCurrentPlayer : Maybe BPlayer
                            maybeCurrentPlayer =
                                List.Extra.find ((==) sessionId << .sessionId) players
                        in
                        case maybeCardToDiscard of
                            Just cardToDiscard ->
                                let
                                    newDiscardPile : DiscardPile
                                    newDiscardPile =
                                        cardToDiscard :: discardPile

                                    newGameStatus : BGameStatus
                                    newGameStatus =
                                        case Card.toPower (maybeTamalouOwner == Nothing || List.length players > 2) cardToDiscard of
                                            Just powerCard ->
                                                BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BPlayerToPlay bPlayer (BPlayerHasDiscard powerCard)) False False

                                            Nothing ->
                                                case nextPlayer maybeTamalouOwner sessionId players of
                                                    Just nextPlayer_ ->
                                                        BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BPlayerToPlay nextPlayer_ (BWaitingPlayerAction Nothing)) False False

                                                    Nothing ->
                                                        BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BEndTimerRunning Five) False False

                                    updatedPlayers : List BPlayer
                                    updatedPlayers =
                                        List.map
                                            (\p ->
                                                if p.sessionId == sessionId then
                                                    { p
                                                        | tableHand =
                                                            List.indexedMap
                                                                (\index card ->
                                                                    if index == cardIndex then
                                                                        { cardInHand | show = False }

                                                                    else
                                                                        card
                                                                )
                                                                p.tableHand
                                                    }

                                                else
                                                    p
                                            )
                                            players
                                in
                                ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) (Just <| AnimationReplaceCardInTableHand sessionId cardIndex cardToDiscard)) updatedPlayers
                                )

                            Nothing ->
                                ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DoubleCardInTableHandToBackend cardIndex ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players bGameInProgressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
                    let
                        canTryToDouble : Bool
                        canTryToDouble =
                            (maybeTamalouOwner /= Just sessionId)
                                && (case bGameInProgressStatus of
                                        BStartTimerRunning _ ->
                                            False

                                        BPlayerToPlay bPlayer bPlayerToPlayStatus ->
                                            let
                                                hasPlayerDrawn : Bool
                                                hasPlayerDrawn =
                                                    case bPlayerToPlayStatus of
                                                        BPlayerHasDrawn _ ->
                                                            True

                                                        _ ->
                                                            False

                                                isPlayerTurn : Bool
                                                isPlayerTurn =
                                                    sessionId == bPlayer.sessionId
                                            in
                                            not isPlayerTurn || (isPlayerTurn && not hasPlayerDrawn)

                                        BEndTimerRunning _ ->
                                            True
                                   )

                        cardFromPlayer : Maybe Card
                        cardFromPlayer =
                            currentPlayer
                                |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)

                        currentPlayer : Maybe BPlayer
                        currentPlayer =
                            List.Extra.find ((==) sessionId << .sessionId) players
                    in
                    case ( canTryToDouble, cardFromPlayer ) of
                        ( True, Just card ) ->
                            case discardPile of
                                discardPileHead :: _ ->
                                    let
                                        maybeMatchingCard : Maybe Card
                                        maybeMatchingCard =
                                            if card.rank == discardPileHead.rank then
                                                Just card

                                            else
                                                Nothing
                                    in
                                    case ( lastMoveIsDouble, maybeMatchingCard ) of
                                        ( False, Just matchingCard ) ->
                                            let
                                                ( updatedPlayers, isGameFinished ) =
                                                    players
                                                        |> List.map
                                                            (\p ->
                                                                if p.sessionId == sessionId then
                                                                    { p
                                                                        | tableHand =
                                                                            p.tableHand
                                                                                |> List.Extra.removeAt cardIndex
                                                                    }

                                                                else
                                                                    p
                                                            )
                                                        |> List.foldr
                                                            (\p ( acc, isFinished ) ->
                                                                ( p :: acc, isFinished || List.isEmpty p.tableHand )
                                                            )
                                                            ( [], False )

                                                newGameStatus : BGameStatus
                                                newGameStatus =
                                                    if isGameFinished then
                                                        let
                                                            orderedPlayers : List ( BPlayer, Int )
                                                            orderedPlayers =
                                                                updatedPlayers
                                                                    |> assignRanks maybeTamalouOwner
                                                                    |> showAllCardsOfAllPlayers
                                                        in
                                                        BGameEnded orderedPlayers

                                                    else
                                                        let
                                                            newDiscardPile : DiscardPile
                                                            newDiscardPile =
                                                                { matchingCard | show = True } :: discardPile
                                                        in
                                                        BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers bGameInProgressStatus True canUsePowerFromLastPlayer
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) (Just <| AnimationDoubleCardSuccess sessionId cardIndex card)) updatedPlayers
                                            )

                                        _ ->
                                            case drawCardFromDrawPile game of
                                                ( Just cardDrew, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                                    let
                                                        newGameStatus : BGameStatus
                                                        newGameStatus =
                                                            BGameInProgress maybeTamalouOwner newDrawPile newDiscardPile updatedPlayers bGameInProgressStatus lastMoveIsDouble canUsePowerFromLastPlayer

                                                        updatedPlayers : List BPlayer
                                                        updatedPlayers =
                                                            players
                                                                |> List.map
                                                                    (\p ->
                                                                        if p.sessionId == sessionId then
                                                                            { p | tableHand = p.tableHand ++ [ { cardDrew | show = False } ] }

                                                                        else
                                                                            p
                                                                    )
                                                    in
                                                    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) (Just <| AnimationDoubleCardFailed sessionId cardIndex card)) updatedPlayers
                                                    )

                                                ( Nothing, _ ) ->
                                                    -- TODO: Terminate game because this should not happen, we need to log this...
                                                    let
                                                        newGameStatus : BGameStatus
                                                        newGameStatus =
                                                            BGameEnded (assignRanks maybeTamalouOwner (stopDisplayCards maybeTamalouOwner players))
                                                    in
                                                    updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LookAtCardInTableHandToBackend cardIndex ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerLookACard ChooseCardToLook)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                BGameInProgress maybeTamalouOwner drawPile discardPile newPlayers (BPlayerToPlay bPlayer (BPlayerLookACard (LookingACard cardIndex Two))) lastMoveIsDouble canUsePowerFromLastPlayer

                            newPlayers : List BPlayer
                            newPlayers =
                                players
                                    |> List.map
                                        (\p ->
                                            if p.sessionId == sessionId then
                                                { p
                                                    | tableHand =
                                                        p.tableHand
                                                            |> List.indexedMap
                                                                (\index card ->
                                                                    if index == cardIndex then
                                                                        { card | show = True }

                                                                    else
                                                                        card
                                                                )
                                                }

                                            else
                                                p
                                        )
                        in
                        ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) Nothing) newPlayers
                        )

                    else
                        ( { model | errors = "1: LookAtCardInTableHandToBackend" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                bGameStatus ->
                    ( { model | errors = ("2: " ++ Debuggy.Logs.bGameInProgressLogs "LookAtCardInTableHandToBackend" bGameStatus) :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

        ChooseOwnCardToSwitchToBackend cardIndex ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards ChooseOwnCardToSwitch)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OwnCardChosen cardIndex))) lastMoveIsDouble canUsePowerFromLastPlayer
                        in
                        updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChooseOpponentCardToSwitchToBackend ( opponentSessionId, opponentCardIndex ) ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OwnCardChosen cardIndex))) lastMoveIsDouble canUsePowerFromLastPlayer ->
                    let
                        newGameStatus : BGameStatus
                        newGameStatus =
                            BGameInProgress maybeTamalouOwner
                                drawPile
                                discardPile
                                newPlayers
                                (BPlayerToPlay bPlayer <|
                                    BPlayerSwitch2Cards (OpponentCardChosen cardIndex { sessionId = opponentSessionId, index = opponentCardIndex } Two)
                                )
                                lastMoveIsDouble
                                canUsePowerFromLastPlayer

                        newPlayers : List BPlayer
                        newPlayers =
                            players
                                |> List.map
                                    (\p ->
                                        if p.sessionId == bPlayer.sessionId then
                                            { p
                                                | tableHand =
                                                    p.tableHand
                                                        |> List.indexedMap
                                                            (\index card ->
                                                                if index == cardIndex then
                                                                    Maybe.withDefault card opponentCard

                                                                else
                                                                    card
                                                            )
                                            }

                                        else if p.sessionId == opponentSessionId then
                                            { p
                                                | tableHand =
                                                    p.tableHand
                                                        |> List.indexedMap
                                                            (\index card ->
                                                                if index == opponentCardIndex then
                                                                    Maybe.withDefault card ownCard

                                                                else
                                                                    card
                                                            )
                                            }

                                        else
                                            p
                                    )

                        opponentCard : Maybe Card
                        opponentCard =
                            players |> List.Extra.find ((==) opponentSessionId << .sessionId) |> Maybe.andThen (\p -> List.Extra.getAt opponentCardIndex p.tableHand)

                        ownCard : Maybe Card
                        ownCard =
                            bPlayer.tableHand |> List.Extra.getAt cardIndex
                    in
                    updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                _ ->
                    ( model, Cmd.none )

        PowerIsUsedToBackend ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerHasDiscard powerCard)) lastMoveIsDouble _ ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                case powerCard of
                                    Card.PlayAgain ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble True

                                    Card.Switch2Cards ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards ChooseOwnCardToSwitch)) lastMoveIsDouble True

                                    Card.LookACard ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerLookACard ChooseCardToLook)) lastMoveIsDouble True
                        in
                        updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                    else
                        ( model, Cmd.none )

                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble True ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                case powerCard of
                                    Card.PlayAgain ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BWaitingPlayerAction Nothing)) lastMoveIsDouble False

                                    Card.Switch2Cards ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards ChooseOwnCardToSwitch)) lastMoveIsDouble False

                                    Card.LookACard ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerLookACard ChooseCardToLook)) lastMoveIsDouble False
                        in
                        updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                    else
                        ( { model | errors = "1: PowerIsUsedToBackend: Wrong player playing" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                bGameStatus ->
                    ( { model | errors = ("2: " ++ Debuggy.Logs.bGameInProgressLogs "PowerIsUsedToBackend: " bGameStatus) :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

        PowerIsNotUsedToBackend ->
            case game.status of
                BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerHasDiscard _)) lastMoveIsDouble _ ->
                    if sessionId == bPlayer.sessionId then
                        let
                            newGameStatus : BGameStatus
                            newGameStatus =
                                case nextPlayer maybeTamalouOwner bPlayer.sessionId players of
                                    Just nextPlayer_ ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay nextPlayer_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble False

                                    Nothing ->
                                        BGameInProgress maybeTamalouOwner drawPile discardPile players (BEndTimerRunning Five) lastMoveIsDouble False
                        in
                        updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                    else
                        ( { model | errors = "1: PowerIsNotUsedToBackend: Wrong player playing" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                bGameStatus ->
                    ( { model | errors = ("2: " ++ Debuggy.Logs.bGameInProgressLogs "PowerIsNotUsedToBackend: " bGameStatus) :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

        TamalouToBackend ->
            case game.status of
                BGameInProgress Nothing drawPile discardPile players (BPlayerToPlay bPlayer (BWaitingPlayerAction _)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                    if sessionId == bPlayer.sessionId then
                        let
                            addPenalty : Bool
                            addPenalty =
                                case currentPlayer of
                                    Just p ->
                                        p.tableHand
                                            |> handIsLessThanFive
                                            |> not

                                    Nothing ->
                                        False

                            -- For now, let's just add a penalty if the current player hand is not tamalou using handScore and handIsLessThanFive functions
                            currentPlayer : Maybe BPlayer
                            currentPlayer =
                                List.Extra.find ((==) sessionId << .sessionId) players

                            nextPlayer_ : BPlayer
                            nextPlayer_ =
                                nextPlayer Nothing bPlayer.sessionId players |> Maybe.withDefault bPlayer
                        in
                        if addPenalty then
                            case drawCardFromDrawPile game of
                                ( Just singleCard, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                    let
                                        newGameStatus : BGameStatus
                                        newGameStatus =
                                            BGameInProgress Nothing newDrawPile newDiscardPile updatedPlayers (BPlayerToPlay nextPlayer_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble canUsePowerFromLastPlayer

                                        updatedPlayers : List BPlayer
                                        updatedPlayers =
                                            players
                                                |> List.map
                                                    (\p ->
                                                        if p.sessionId == sessionId then
                                                            { p
                                                                | tableHand =
                                                                    p.tableHand
                                                                        ++ [ { singleCard | show = False } ]
                                                            }

                                                        else
                                                            p
                                                    )
                                    in
                                    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) Nothing) updatedPlayers
                                    )

                                ( Nothing, _ ) ->
                                    -- TODO: Terminate game because this should not happen, we need to log this...
                                    let
                                        newGameStatus : BGameStatus
                                        newGameStatus =
                                            BGameEnded (assignRanks (Just sessionId) (stopDisplayCards (Just sessionId) players))
                                    in
                                    updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players Nothing

                        else
                            let
                                newGameStatus : BGameStatus
                                newGameStatus =
                                    BGameInProgress (Just sessionId) drawPile discardPile updatedPlayers (BPlayerToPlay nextPlayer_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble canUsePowerFromLastPlayer

                                updatedPlayers : List BPlayer
                                updatedPlayers =
                                    players
                                        |> List.map
                                            (\p ->
                                                if p.sessionId == sessionId then
                                                    { p | tableHand = p.tableHand |> List.map (\card -> { card | show = False }) }

                                                else
                                                    p
                                            )
                            in
                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) Nothing) updatedPlayers
                            )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SendMessageToBackend newMessage ->
            let
                newChat : List ( String, String )
                newChat =
                    game.chat ++ [ ( playerName sessionId game, newMessage ) ]

                newGame : BGame
                newGame =
                    { game | chat = newChat }
            in
            ( { model | games = updateGame newGame games }
            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateChatToFrontend newChat) (bPlayersFromFGame newGame)
            )


updateGameStateAndNotifyPlayers : BackendModel -> String -> ( BGameStatus, Random.Seed ) -> List BPlayer -> Maybe PlayerActionAnimation -> ( BackendModel, Cmd BackendMsg )
updateGameStateAndNotifyPlayers ({ games } as model) urlPath ( newGameStatus, newSeed ) players maybePlayerAction =
    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) maybePlayerAction) players
    )
