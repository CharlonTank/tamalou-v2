module Backend exposing (..)

import Card exposing (Card, FCard(..), handIsLessThanFive)
import DebugApp
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Maybe.Extra as Maybe
import Random
import Random.Extra as Random
import Random.List as Random
import Task
import Time
import Types exposing (..)


app : { init : ( BackendModel, Cmd BackendMsg ), update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg ), updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg ), subscriptions : BackendModel -> Sub BackendMsg }
app =
    -- DebugApp.backend
    --     NoOpBackendMsg
    --     "8c7b7b0c1fc0357e"
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions { games } =
    Sub.batch
        [ Lamdera.onDisconnect GotUserDisconnected
        , Lamdera.onConnect FeedSessionIdAndClientId
        , launchTimer games
        ]


launchTimer : List BGame -> Sub BackendMsg
launchTimer games =
    games
        |> List.map
            (\g ->
                case g.status of
                    BGameInProgress _ _ _ _ (BTimerRunning _) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    _ ->
                        Sub.none
            )
        |> Sub.batch


shuffleWithSeed : Random.Seed -> List a -> ( List a, Random.Seed )
shuffleWithSeed initialSeed list =
    let
        ( taggedList, finalSeed ) =
            list
                |> List.foldl
                    (\item ( acc, currentSeed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step (Random.int Random.minInt Random.maxInt) currentSeed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], initialSeed )

        shuffledList =
            taggedList
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
    in
    ( shuffledList, finalSeed )


shuffleList : List a -> Random.Generator (List a)
shuffleList list =
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step (Random.int Random.minInt Random.maxInt) seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { games = [] }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg ({ games } as model) =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        FeedSessionIdAndClientId sessionId clientId ->
            ( model, Lamdera.sendToFrontend clientId (GotSessionIdAndClientId sessionId clientId) )

        GotUserDisconnected _ clientId ->
            let
                ( updatedGames, cmds ) =
                    games
                        |> List.map
                            (\game ->
                                case game.status of
                                    BWaitingForPlayers players ->
                                        let
                                            newPlayers =
                                                List.filter ((/=) clientId << .clientId) players

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers
                                        in
                                        if List.length newPlayers /= List.length players then
                                            ( { game | status = newGameStatus }
                                            , List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) newPlayers
                                            )

                                        else
                                            ( game, [] )

                                    BGameInProgress _ _ _ _ _ _ _ ->
                                        ( game, [] )

                                    BGameEnded _ ->
                                        ( game, [] )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond List.concat
            in
            ( { model | games = updatedGames }, Cmd.batch cmds )

        BackendMsgFromGame urlPath toBackend ->
            let
                maybeGame =
                    List.Extra.find ((==) urlPath << .urlPath) games
            in
            case toBackend of
                TimerTick posix ->
                    case maybeGame of
                        Just game ->
                            case game.status of
                                BGameInProgress m a b (p1 :: restOfPlayers) (BTimerRunning nb) lastMoveIsDouble powerAlreadyUsed ->
                                    let
                                        maybeShuffleDrawAndNewSeed =
                                            if nb == 5 then
                                                Just <| shuffleWithSeed (Random.initialSeed (posix |> Time.posixToMillis)) Card.nonShuffledDeck

                                            else
                                                Nothing

                                        newGame =
                                            { game
                                                | status =
                                                    if (nb - 1) < 0 then
                                                        BGameInProgress m a b (stopDisplayCards (p1 :: restOfPlayers)) (BPlayerToPlay p1.sessionId (BWaitingPlayerAction Nothing)) lastMoveIsDouble powerAlreadyUsed

                                                    else
                                                        BGameInProgress m a b (p1 :: restOfPlayers) (BTimerRunning <| nb - 1) lastMoveIsDouble powerAlreadyUsed
                                            }
                                                |> (\g ->
                                                        case maybeShuffleDrawAndNewSeed of
                                                            Just ( newDrawPile, newSeed ) ->
                                                                let
                                                                    ( newDrawPile_, newPlayers ) =
                                                                        List.foldl
                                                                            (\player ( drawPile_, players_ ) ->
                                                                                let
                                                                                    ( newDrawPile__, newPlayer ) =
                                                                                        distribute4CardsToPlayer drawPile_ player
                                                                                in
                                                                                ( newDrawPile__, players_ ++ [ newPlayer ] )
                                                                            )
                                                                            ( newDrawPile, [] )
                                                                            (p1 :: restOfPlayers)

                                                                    newGame_ =
                                                                        BGameInProgress m newDrawPile_ [] newPlayers (BTimerRunning 4) False powerAlreadyUsed
                                                                in
                                                                { g | seed = newSeed, status = newGame_ }

                                                            Nothing ->
                                                                g
                                                   )
                                    in
                                    ( { model | games = updateGame newGame games }
                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGame.status) (p1 :: restOfPlayers)
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


stopDisplayCards : List BPlayer -> List BPlayer
stopDisplayCards players =
    List.map
        (\({ tableHand } as player) ->
            { player | tableHand = List.map (\card -> { card | show = False }) tableHand }
        )
        players


distribute4CardsToPlayer : BDrawPile -> BPlayer -> ( BDrawPile, BPlayer )
distribute4CardsToPlayer drawPile player =
    case drawPile of
        [] ->
            ( drawPile, player )

        card1 :: [] ->
            ( [], { player | tableHand = [ { card1 | show = True } ] } )

        card1 :: card2 :: [] ->
            ( [], { player | tableHand = [ { card1 | show = True }, card2 ] } )

        card1 :: card2 :: card3 :: [] ->
            ( [], { player | tableHand = [ { card1 | show = True }, card2, card3 ] } )

        card1 :: card2 :: card3 :: card4 :: drawPile_ ->
            ( drawPile_, { player | tableHand = [ { card1 | show = True }, card2, card3, { card4 | show = True } ] } )


updateGameStatus : String -> ( BGameStatus, Random.Seed ) -> List BGame -> List BGame
updateGameStatus urlPath ( newGameStatus, newSeed ) games =
    List.Extra.updateIf
        ((==) urlPath << .urlPath)
        (\g -> { g | status = newGameStatus, seed = newSeed })
        games


updateGame : BGame -> List BGame -> List BGame
updateGame newGame games =
    List.Extra.updateIf
        ((==) newGame.urlPath << .urlPath)
        (always newGame)
        games


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg ({ games } as model) =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        ToBackendActionFromGame urlPath toBackendActionFromGame ->
            if urlPath == "/reload" then
                init

            else
                let
                    maybeGame =
                        List.Extra.find ((==) urlPath << .urlPath) games
                in
                case toBackendActionFromGame of
                    ConnectToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BWaitingForPlayers players ->
                                        let
                                            newPlayers =
                                                case List.Extra.find ((==) sessionId << .sessionId) players of
                                                    Just _ ->
                                                        players

                                                    Nothing ->
                                                        players
                                                            ++ [ { name = "Player " ++ String.fromInt (List.length players + 1)
                                                                 , tableHand = []
                                                                 , clientId = clientId
                                                                 , sessionId = sessionId
                                                                 }
                                                               ]

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers

                                            frontendGame : FGame
                                            frontendGame =
                                                backendGameStatusToFrontendGame Nothing newGameStatus
                                        in
                                        case List.length newPlayers of
                                            1 ->
                                                ( { model | games = updateGameStatus urlPath ( BWaitingForPlayers newPlayers, game.seed ) games }
                                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGame frontendGame)) newPlayers
                                                )

                                            2 ->
                                                ( { model | games = updateGameStatus urlPath ( BWaitingForPlayers newPlayers, game.seed ) games }
                                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGame frontendGame)) newPlayers
                                                )

                                            -- 3 ->
                                            --     ( { model | games = updateGameStatus urlPath ( BWaitingForPlayers newPlayers, game.seed ) games }
                                            --     , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGame frontendGame)) newPlayers
                                            --     )
                                            3 ->
                                                ( { model | games = updateGameStatus urlPath ( BGameInProgress Nothing [] [] newPlayers (BTimerRunning 5) False (PowerAlreadyUsed False), game.seed ) games }
                                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGame frontendGame)) newPlayers
                                                )

                                            _ ->
                                                ( model
                                                , Cmd.none
                                                )

                                    BGameInProgress maybeTamalouOwner drawPile discardPile players progressStatus lastMoveIsDouble powerAlreadyUsed ->
                                        case List.Extra.find ((==) sessionId << .sessionId) players of
                                            Just _ ->
                                                let
                                                    updateClientIdInPlayers =
                                                        List.map
                                                            (\p ->
                                                                if p.sessionId == sessionId then
                                                                    { p | clientId = clientId }

                                                                else
                                                                    p
                                                            )
                                                            players

                                                    newGame =
                                                        BGameInProgress maybeTamalouOwner drawPile discardPile updateClientIdInPlayers progressStatus lastMoveIsDouble powerAlreadyUsed
                                                in
                                                ( { model | games = updateGameStatus urlPath ( newGame, game.seed ) games }
                                                , Cmd.batch <|
                                                    List.map (\p -> Lamdera.sendToFrontend p.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just p.sessionId) newGame) updateClientIdInPlayers
                                                )

                                            Nothing ->
                                                ( model, Lamdera.sendToFrontend clientId (UpdateGame FGameAlreadyStartedWithoutYou) )

                                    BGameEnded _ ->
                                        ( model
                                        , Cmd.none
                                        )

                            Nothing ->
                                let
                                    newGame =
                                        { urlPath = urlPath, seed = Random.initialSeed 0, status = BWaitingForPlayers [ { name = "Player 1", tableHand = [], clientId = clientId, sessionId = sessionId } ] }
                                in
                                ( { model | games = newGame :: games }
                                , Lamdera.sendToFrontend clientId (UpdateGame (backendGameStatusToFrontendGame Nothing newGame.status))
                                )

                    DrawCardFromDrawPileToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner _ _ players (BPlayerToPlay sessionId_ (BWaitingPlayerAction _)) lastMoveIsDouble powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            case drawCardFromDrawPile game of
                                                ( Just cardDrew, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                                    let
                                                        newGameStatus =
                                                            BGameInProgress maybeTamalouOwner newDrawPile newDiscardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw cardDrew)) lastMoveIsDouble powerAlreadyUsed
                                                    in
                                                    updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, newSeed ) players

                                                ( Nothing, _ ) ->
                                                    ( model, Cmd.none )

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    DrawOrUsePowerFromDiscardPileToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile (head :: rest) players (BPlayerToPlay sessionId_ (BWaitingPlayerAction _)) _ powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile rest players (BPlayerToPlay sessionId_ (BPlayerHasDraw head)) False powerAlreadyUsed
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) players
                                            )

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    DiscardCardInHandToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw card)) _ powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                isGameFinished =
                                                    maybeTamalouOwner == Just (nextPlayerSessionId sessionId_ players)

                                                newGameStatus =
                                                    case Card.toPower card of
                                                        Just powerCard ->
                                                            BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BPlayerToPlay sessionId_ (BPlayerHasDiscard powerCard)) False powerAlreadyUsed

                                                        Nothing ->
                                                            if isGameFinished then
                                                                BGameEnded clientId

                                                            else
                                                                BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BPlayerToPlay (nextPlayerSessionId sessionId_ players) (BWaitingPlayerAction Nothing)) False powerAlreadyUsed
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) players
                                            )

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    ReplaceCardInTableHandToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw cardInHand)) _ powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                currentPlayer =
                                                    List.Extra.find ((==) sessionId << .sessionId) players

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

                                                cardToDiscard =
                                                    currentPlayer |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)

                                                newDiscardPile =
                                                    cardToDiscard |> Maybe.map (\c -> { c | show = True }) |> Maybe.map (\c -> c :: discardPile) |> Maybe.withDefault discardPile

                                                isGameFinished =
                                                    maybeTamalouOwner == Just (nextPlayerSessionId sessionId_ players)

                                                newGameStatus =
                                                    case Maybe.andThen Card.toPower cardToDiscard of
                                                        Just powerCard ->
                                                            BGameInProgress maybeTamalouOwner drawPile newDiscardPile players (BPlayerToPlay sessionId_ (BPlayerHasDiscard powerCard)) False powerAlreadyUsed

                                                        Nothing ->
                                                            if isGameFinished then
                                                                BGameEnded clientId

                                                            else
                                                                BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BPlayerToPlay (nextPlayerSessionId sessionId_ players) (BWaitingPlayerAction Nothing)) False powerAlreadyUsed
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) updatedPlayers
                                            )

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    DoubleCardInTableHandToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ bPlayerToPlayStatus) doubleIsLastMove powerAlreadyUsed ->
                                        let
                                            isPlayerTurn =
                                                sessionId == sessionId_

                                            hasPlayerDrawn =
                                                case bPlayerToPlayStatus of
                                                    BPlayerHasDraw _ ->
                                                        True

                                                    _ ->
                                                        False
                                        in
                                        if not isPlayerTurn || (isPlayerTurn && not hasPlayerDrawn) then
                                            case discardPile of
                                                discardPileHead :: _ ->
                                                    let
                                                        currentPlayer =
                                                            List.Extra.find ((==) sessionId << .sessionId) players

                                                        maybeMatchingCard : Maybe Card
                                                        maybeMatchingCard =
                                                            currentPlayer
                                                                |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)
                                                                |> Maybe.andThen
                                                                    (\card ->
                                                                        if card.rank == discardPileHead.rank then
                                                                            Just card

                                                                        else
                                                                            Nothing
                                                                    )
                                                    in
                                                    case ( doubleIsLastMove, maybeMatchingCard ) of
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

                                                                newDiscardPile =
                                                                    { matchingCard | show = True } :: discardPile

                                                                newGameStatus =
                                                                    if isGameFinished then
                                                                        BGameEnded sessionId

                                                                    else
                                                                        BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BPlayerToPlay sessionId_ bPlayerToPlayStatus) True powerAlreadyUsed
                                                            in
                                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) updatedPlayers
                                                            )

                                                        _ ->
                                                            case drawCardFromDrawPile game of
                                                                ( Just cardDrew, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                                                    let
                                                                        updatedPlayers =
                                                                            players
                                                                                |> List.map
                                                                                    (\p ->
                                                                                        if p.sessionId == sessionId then
                                                                                            { p
                                                                                                | tableHand =
                                                                                                    p.tableHand
                                                                                                        ++ [ { cardDrew | show = False } ]
                                                                                            }

                                                                                        else
                                                                                            p
                                                                                    )

                                                                        newGameStatus =
                                                                            BGameInProgress maybeTamalouOwner newDrawPile newDiscardPile updatedPlayers (BPlayerToPlay sessionId_ bPlayerToPlayStatus) False powerAlreadyUsed
                                                                    in
                                                                    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) updatedPlayers
                                                                    )

                                                                ( Nothing, _ ) ->
                                                                    ( model, Cmd.none )

                                                _ ->
                                                    ( model, Cmd.none )

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    TamalouToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress Nothing drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction _)) lastMoveIsDouble powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                currentPlayer =
                                                    List.Extra.find ((==) sessionId << .sessionId) players

                                                -- For now, let's just add a penalty if the current player hand is not tamalou using handScore and handIsLessThanFive functions
                                                addPenalty : Bool
                                                addPenalty =
                                                    case currentPlayer of
                                                        Just p ->
                                                            p.tableHand
                                                                |> handIsLessThanFive
                                                                |> not

                                                        Nothing ->
                                                            False
                                            in
                                            if addPenalty then
                                                case drawCardFromDrawPile game of
                                                    ( Just singleCard, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                                        let
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

                                                            newGameStatus =
                                                                BGameInProgress Nothing newDrawPile newDiscardPile updatedPlayers (BPlayerToPlay (nextPlayerSessionId sessionId_ updatedPlayers) (BWaitingPlayerAction Nothing)) lastMoveIsDouble powerAlreadyUsed
                                                        in
                                                        ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) updatedPlayers
                                                        )

                                                    ( Nothing, _ ) ->
                                                        ( model, Cmd.none )

                                            else
                                                let
                                                    updatedPlayers =
                                                        players
                                                            |> List.map
                                                                (\p ->
                                                                    if p.sessionId == sessionId then
                                                                        { p
                                                                            | tableHand =
                                                                                p.tableHand
                                                                                    |> List.map (\card -> { card | show = False })
                                                                        }

                                                                    else
                                                                        p
                                                                )

                                                    newGameStatus =
                                                        BGameInProgress (Just sessionId) drawPile discardPile updatedPlayers (BPlayerToPlay (nextPlayerSessionId sessionId_ updatedPlayers) (BWaitingPlayerAction Nothing)) lastMoveIsDouble powerAlreadyUsed
                                                in
                                                ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) updatedPlayers
                                                )

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    PowerIsUsedToBackend ->
                        -- in this case we will apply the power, to apply the power, we can be in to possible configurations:
                        -- 1. The player just discarded a card so the game status is BPlayerHasDiscard powerCard
                        -- in this case, we update the status to BWaitingPlayerAction (Just powerCard) and we notify the players
                        -- 2. The last player used the power of their powerCard which led the currentPlayer to (BWaitingPlayerAction (Just power)), in this case the user can also use the power and
                        -- This action can only be done if the game status is BPlayerHasDiscard power or BWaitingPlayerAction (Just power). BPlayerHasDiscard will go to BWaitingPlayerAction (Just power) and BWaitingPlayerAction (Just power) will go to BWaitingPlayerAction Nothing
                        -- There are 3 powers :
                        -- 1) Jack is PlayAgain, which will result in setting the game status to BWaitingPlayerAction (Just power)
                        -- 2) Queen is Switch2Cards, which will result in the ability to switch 1 card from the player hand with 1 card from any other player tableHand, for now, we will just do the same as PlayAgain
                        -- 3) Kind is LookACard, which will result in the ability to look 1 card form the player hand for 5 seconds, for now, we will just do the same as PlayAgain
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDiscard powerCard)) lastMoveIsDouble powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble powerAlreadyUsed
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players

                                        else
                                            ( model, Cmd.none )

                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble powerAlreadyUsed
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    PowerIsNotUsedToBackend ->
                        -- in this case we will not apply the power. We will just update the game status to BWaitingPlayerAction Nothing and notify the players
                        -- This action can only be done if the game status is BPlayerHasDiscard powerCard
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDiscard _)) lastMoveIsDouble powerAlreadyUsed ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay (nextPlayerSessionId sessionId_ players) (BWaitingPlayerAction Nothing)) lastMoveIsDouble powerAlreadyUsed
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )


drawCardFromDrawPile : BGame -> ( Maybe Card, ( BDrawPile, DiscardPile, Random.Seed ) )
drawCardFromDrawPile game =
    case game.status of
        BGameInProgress _ drawPile discardPile _ _ _ _ ->
            case ( drawPile, discardPile ) of
                ( [], [] ) ->
                    ( Nothing, ( [], [], game.seed ) )

                ( [], [ singleCard ] ) ->
                    ( Just singleCard, ( [], [], game.seed ) )

                ( [], firstCard :: restOfDiscardPile ) ->
                    let
                        ( shuffledRestDiscardPile, newSeed ) =
                            shuffleWithSeed game.seed restOfDiscardPile
                    in
                    case List.Extra.uncons shuffledRestDiscardPile of
                        Just ( cardDrew, newDrawPile ) ->
                            ( Just cardDrew, ( newDrawPile, [ firstCard ], newSeed ) )

                        Nothing ->
                            ( Nothing, ( [], [ firstCard ], newSeed ) )

                ( [ singleCard ], [] ) ->
                    ( Just singleCard, ( [], [], game.seed ) )

                ( [ singleCard ], firstCard :: restOfDiscardPile ) ->
                    let
                        ( shuffledRestDiscardPile, newSeed ) =
                            shuffleWithSeed game.seed restOfDiscardPile
                    in
                    ( Just singleCard, ( shuffledRestDiscardPile, [ firstCard ], newSeed ) )

                ( firstCard :: restOfDrawPile, _ ) ->
                    ( Just firstCard, ( restOfDrawPile, discardPile, game.seed ) )

        _ ->
            ( Nothing, ( [], [], game.seed ) )


updateGameStateAndNotifyPlayers : BackendModel -> String -> ( BGameStatus, Random.Seed ) -> List BPlayer -> ( BackendModel, Cmd BackendMsg )
updateGameStateAndNotifyPlayers ({ games } as model) urlPath ( newGameStatus, newSeed ) players =
    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameStatusToFrontendGame (Just player.sessionId) newGameStatus) players
    )


nextPlayerSessionId : SessionId -> List BPlayer -> SessionId
nextPlayerSessionId sessionId players =
    List.Extra.findIndex ((==) sessionId << .sessionId) players
        |> Maybe.andThen (\index_ -> List.Extra.getAt (modBy (List.length players) (index_ + 1)) players)
        |> Maybe.map .sessionId
        |> Maybe.withDefault sessionId


backendGameStatusToFrontendGame : Maybe SessionId -> BGameStatus -> FGame
backendGameStatusToFrontendGame maybeSessionId backendGame =
    case backendGame of
        BWaitingForPlayers players ->
            FWaitingForPlayers (List.map backendPlayerToFrontendPlayer players)

        BGameInProgress Nothing bDrawPile discardPile players bGameInProgressStatus _ _ ->
            let
                tableHand =
                    case maybeSessionId of
                        Just sessionId ->
                            List.Extra.find ((==) sessionId << .sessionId) players
                                |> Maybe.map .tableHand
                                |> Maybe.map (List.map cardToFrontendCard)
                                |> Maybe.withDefault [ FaceUp Card.sampleCard, FaceUp Card.sampleCard ]

                        Nothing ->
                            [ FaceUp Card.sampleCard ]
            in
            FGameInProgress Nothing tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile (List.map backendPlayerToFrontendPlayer players) (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameInProgress (Just tamalouOwnerSessionId) bDrawPile discardPile players bGameInProgressStatus _ _ ->
            let
                tableHand =
                    case maybeSessionId of
                        Just sessionId ->
                            List.Extra.find ((==) sessionId << .sessionId) players
                                |> Maybe.map .tableHand
                                |> Maybe.map (List.map cardToFrontendCard)
                                |> Maybe.withDefault [ FaceUp Card.sampleCard, FaceUp Card.sampleCard ]

                        Nothing ->
                            [ FaceUp Card.sampleCard ]

                tamalouOwner =
                    List.Extra.find ((==) tamalouOwnerSessionId << .sessionId) players
                        |> Maybe.map (\p -> TamalouOwner p.sessionId p.tableHand)
            in
            FGameInProgress tamalouOwner tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile (List.map backendPlayerToFrontendPlayer players) (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameEnded clientId_ ->
            FGameEnded clientId_


toFGameProgressStatus : Maybe SessionId -> BGameInProgressStatus -> FGameInProgressStatus
toFGameProgressStatus maybeSessionId bGameInProgressStatus =
    case bGameInProgressStatus of
        BTimerRunning timer ->
            FTimerRunning timer

        BPlayerToPlay sessionId bPlayerToPlayStatus ->
            case bPlayerToPlayStatus of
                BWaitingPlayerAction maybePowerCard ->
                    if maybeSessionId == Just sessionId then
                        FYourTurn (FWaitingPlayerAction maybePowerCard)

                    else
                        FPlayerToPlay sessionId (FWaitingPlayerAction maybePowerCard)

                BPlayerHasDraw card ->
                    if maybeSessionId == Just sessionId then
                        FYourTurn (FPlayerHasDraw (FaceUp card))

                    else
                        FPlayerToPlay sessionId (FPlayerHasDraw FaceDown)

                BPlayerHasDiscard powerCard ->
                    if maybeSessionId == Just sessionId then
                        FYourTurn (FPlayerHasDiscard powerCard)

                    else
                        FPlayerToPlay sessionId (FPlayerHasDiscard powerCard)


backendPlayerToFrontendPlayer : BPlayer -> FPlayer
backendPlayerToFrontendPlayer backendPlayer =
    { name = backendPlayer.name
    , tableHand = List.map cardToFrontendCard backendPlayer.tableHand
    , clientId = backendPlayer.clientId
    , sessionId = backendPlayer.sessionId
    }


cardToFrontendCard : Card -> FCard
cardToFrontendCard card =
    if card.show then
        FaceUp card

    else
        FaceDown
