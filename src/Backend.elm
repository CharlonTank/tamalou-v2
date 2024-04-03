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
    --     "e465a26049dfca11"
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
                    BGameInProgress _ _ _ _ (BStartTimerRunning _) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    BGameInProgress _ _ _ _ (BEndTimerRunning _) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    BGameInProgress _ _ _ _ (BPlayerToPlay _ (BPlayerLookACard (Just _))) _ _ ->
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
    ( { games = [], errors = [], admins = [] }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg ({ games } as model) =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        FeedSessionIdAndClientId sessionId clientId ->
            ( model, Lamdera.sendToFrontend clientId (GotSessionIdAndClientIdToFrontend sessionId clientId) )

        GotUserDisconnected sessionId _ ->
            let
                ( updatedGames, cmds ) =
                    games
                        |> List.map
                            (\game ->
                                case game.status of
                                    BWaitingForPlayers players ->
                                        let
                                            newPlayers =
                                                List.filter ((/=) sessionId << .sessionId) players

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers
                                        in
                                        if List.length newPlayers /= List.length players then
                                            ( { game | status = newGameStatus }
                                            , List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame Nothing newGameStatus) newPlayers
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
                TimerTick _ ->
                    case maybeGame of
                        Just game ->
                            case game.status of
                                BGameInProgress m a b (p1 :: restOfBPlayers) (BStartTimerRunning nb) _ _ ->
                                    let
                                        newNb =
                                            decrementCounter nb

                                        newGame =
                                            { game
                                                | status =
                                                    case newNb of
                                                        Nothing ->
                                                            BGameInProgress m a b (stopDisplayCards (p1 :: restOfBPlayers)) (BPlayerToPlay p1.sessionId (BWaitingPlayerAction Nothing)) False False

                                                        Just newNb_ ->
                                                            BGameInProgress m a b (p1 :: restOfBPlayers) (BStartTimerRunning newNb_) False False
                                            }
                                    in
                                    ( { model | games = updateGame newGame games }
                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGame.status) (p1 :: restOfBPlayers)
                                    )

                                BGameInProgress m a b (p1 :: restOfBPlayers) (BEndTimerRunning nb) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                    case decrementCounter nb of
                                        Just nb_ ->
                                            let
                                                newGame =
                                                    { game | status = BGameInProgress m a b (p1 :: restOfBPlayers) (BEndTimerRunning nb_) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGame.status) (p1 :: restOfBPlayers)
                                            )

                                        Nothing ->
                                            let
                                                orderedPlayers =
                                                    (p1 :: restOfBPlayers)
                                                        |> List.sortBy
                                                            (\player ->
                                                                Card.tableHandScore player.tableHand
                                                            )
                                                        |> showAllCardsOfAllPlayers
                                            in
                                            let
                                                newGame =
                                                    { game | status = BGameEnded orderedPlayers }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGame.status) (p1 :: restOfBPlayers)
                                            )

                                BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay sessionId_ (BPlayerLookACard maybeNb)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                    case Maybe.andThen decrementCounter maybeNb of
                                        Just nb ->
                                            let
                                                newGame =
                                                    { game | status = BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay sessionId_ (BPlayerLookACard (Just nb))) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGame.status) players
                                            )

                                        Nothing ->
                                            let
                                                maybeNextPlayerSessionId =
                                                    nextPlayerSessionId maybeTamalouOwner sessionId_ players

                                                newGame =
                                                    case maybeNextPlayerSessionId of
                                                        Just nextPlayerSessionId_ ->
                                                            { game
                                                                | status =
                                                                    BGameInProgress maybeTamalouOwner
                                                                        a
                                                                        b
                                                                        (stopDisplayCards players)
                                                                        (BPlayerToPlay nextPlayerSessionId_
                                                                            (BWaitingPlayerAction
                                                                                (if canUsePowerFromLastPlayer then
                                                                                    Just Card.LookACard

                                                                                 else
                                                                                    Nothing
                                                                                )
                                                                            )
                                                                        )
                                                                        lastMoveIsDouble
                                                                        canUsePowerFromLastPlayer
                                                            }

                                                        Nothing ->
                                                            { game
                                                                | status =
                                                                    BGameInProgress maybeTamalouOwner a b (stopDisplayCards players) (BEndTimerRunning Five) lastMoveIsDouble canUsePowerFromLastPlayer
                                                            }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGame.status) players
                                            )

                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                CreateGame posix clientId sessionId ->
                    let
                        ( funnyName, newSeed ) =
                            generateRandomFunnyName (Random.initialSeed (posix |> Time.posixToMillis)) []

                        newGame =
                            { urlPath = urlPath
                            , status = BWaitingForPlayers [ { name = funnyName, tableHand = [], clientId = clientId, sessionId = sessionId, ready = False } ]
                            , chat = []
                            , seed = newSeed
                            }
                    in
                    ( { model | games = newGame :: games }
                    , Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend (toFGame (Just sessionId) newGame.status))
                    )


showAllCardsOfAllPlayers : List BPlayer -> List BPlayer
showAllCardsOfAllPlayers players =
    List.map
        (\({ tableHand } as player) ->
            { player | tableHand = List.map (\card -> { card | show = True }) tableHand }
        )
        players


decrementCounter : Counter -> Maybe Counter
decrementCounter counter =
    case counter of
        Five ->
            Just Four

        Four ->
            Just Three

        Three ->
            Just Two

        Two ->
            Just One

        One ->
            Just Zero

        Zero ->
            Nothing


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
            -- Debug.log "distribute4CardsToPlayer" ( drawPile_, { player | tableHand = [ { card1 | show = True } ] } )
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
updateFromFrontend sessionId clientId msg ({ games, errors } as model) =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        ConnectToAdminToBackend ->
            ( model, Lamdera.sendToFrontend clientId (UpdateAdminToFrontend errors) )

        ActionFromGameToBackend urlPath toBackendActionFromGame ->
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
                                            ( funnyName, newSeed ) =
                                                generateRandomFunnyName game.seed (List.map .name players)

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

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers

                                            frontendGame : FGame
                                            frontendGame =
                                                toFGame Nothing newGameStatus
                                        in
                                        ( { model | games = updateGameStatus urlPath ( BWaitingForPlayers newPlayers, newSeed ) games }
                                        , Cmd.batch <|
                                            Lamdera.sendToFrontend clientId (UpdateChatToFrontend game.chat)
                                                :: List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame)) newPlayers
                                        )

                                    BGameInProgress maybeTamalouOwner drawPile discardPile players progressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
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
                                                        BGameInProgress maybeTamalouOwner drawPile discardPile updateClientIdInPlayers progressStatus lastMoveIsDouble canUsePowerFromLastPlayer
                                                in
                                                ( { model | games = updateGameStatus urlPath ( newGame, game.seed ) games }
                                                , Cmd.batch <|
                                                    List.map (\p -> Lamdera.sendToFrontend p.clientId <| UpdateGameStatusToFrontend <| toFGame (Just p.sessionId) newGame) updateClientIdInPlayers
                                                )

                                            Nothing ->
                                                ( model, Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend FGameAlreadyStartedWithoutYou) )

                                    BGameEnded players ->
                                        ( model
                                        , Lamdera.sendToFrontend clientId (UpdateGameAndChatToFrontend ( toFGame Nothing (BGameEnded players), game.chat ))
                                        )

                            Nothing ->
                                ( model
                                , Task.perform (\posix -> BackendMsgFromGame urlPath (CreateGame posix clientId sessionId)) Time.now
                                )

                    ChangeCurrentPlayerNameToBackend newName ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BWaitingForPlayers players ->
                                        let
                                            newPlayers =
                                                List.map
                                                    (\p ->
                                                        if p.sessionId == sessionId then
                                                            { p | name = newName }

                                                        else
                                                            p
                                                    )
                                                    players

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers

                                            frontendGame : FGame
                                            frontendGame =
                                                toFGame Nothing newGameStatus
                                        in
                                        ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame)) newPlayers
                                        )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    ImReadyToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BWaitingForPlayers players ->
                                        let
                                            newPlayers =
                                                List.map
                                                    (\p ->
                                                        if p.sessionId == sessionId then
                                                            { p | ready = True }

                                                        else
                                                            p
                                                    )
                                                    players

                                            allPlayersReady =
                                                List.all .ready newPlayers
                                        in
                                        if allPlayersReady && List.length newPlayers >= 2 then
                                            let
                                                ( newDrawPile, newSeed ) =
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

                                                newGameStatus =
                                                    BGameInProgress Nothing newDrawPile_ [] newPlayers_ (BStartTimerRunning Five) False False

                                                frontendGame : FGame
                                                frontendGame =
                                                    toFGame Nothing newGameStatus
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend frontendGame)) newPlayers
                                            )

                                        else
                                            let
                                                newGameStatus =
                                                    BWaitingForPlayers newPlayers

                                                frontendGame : FGame
                                                frontendGame =
                                                    toFGame Nothing newGameStatus

                                                newChat =
                                                    game.chat ++ [ ( Maybe.withDefault "" (List.Extra.find ((==) sessionId << .sessionId) newPlayers |> Maybe.map .name), "Let's go I'm ready!" ) ]

                                                newGame =
                                                    { game | chat = newChat, status = newGameStatus }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameAndChatToFrontend ( frontendGame, newChat ))) newPlayers
                                            )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    ReStartGameToBackend maybeFPlayer ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameEnded players ->
                                        let
                                            newPlayers =
                                                players
                                                    -- Warning, here we remove all the other players in case they disconnect wihtout clicking restart, In the future, we want to send the score instead of the players so that we can remove them from the game on disconnect
                                                    |> List.filter ((==) sessionId << .sessionId)
                                                    |> List.map
                                                        (\p ->
                                                            { p
                                                                | tableHand = []
                                                                , ready = False
                                                            }
                                                        )

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers

                                            frontendGame : FGame
                                            frontendGame =
                                                toFGame Nothing newGameStatus
                                        in
                                        ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                        , Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend frontendGame)
                                        )

                                    BWaitingForPlayers players ->
                                        let
                                            newPlayers =
                                                players ++ [ maybeFPlayer |> Maybe.map toBPlayer |> Maybe.map (\p -> { p | ready = False }) |> Maybe.withDefault (emptyBPlayer sessionId clientId) ]

                                            newGameStatus =
                                                BWaitingForPlayers newPlayers

                                            frontendGame : FGame
                                            frontendGame =
                                                toFGame Nothing newGameStatus
                                        in
                                        ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                        , Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend frontendGame)
                                        )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    DrawCardFromDrawPileToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner _ _ players (BPlayerToPlay sessionId_ (BWaitingPlayerAction _)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                        if sessionId == sessionId_ then
                                            case drawCardFromDrawPile game of
                                                ( Just cardDrew, ( newDrawPile, newDiscardPile, newSeed ) ) ->
                                                    let
                                                        newGameStatus =
                                                            BGameInProgress maybeTamalouOwner newDrawPile newDiscardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw cardDrew)) lastMoveIsDouble canUsePowerFromLastPlayer
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

                    DrawFromDiscardPileToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile (head :: rest) players (BPlayerToPlay sessionId_ (BWaitingPlayerAction _)) _ _ ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile rest players (BPlayerToPlay sessionId_ (BPlayerHasDraw head)) False False
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) players
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
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw card)) _ _ ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    case Card.toPower card of
                                                        Just powerCard ->
                                                            BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BPlayerToPlay sessionId_ (BPlayerHasDiscard powerCard)) False False

                                                        Nothing ->
                                                            case nextPlayerSessionId maybeTamalouOwner sessionId players of
                                                                Just nextPlayerSessionId_ ->
                                                                    BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BPlayerToPlay nextPlayerSessionId_ (BWaitingPlayerAction Nothing)) False False

                                                                Nothing ->
                                                                    BGameInProgress maybeTamalouOwner drawPile (card :: discardPile) players (BEndTimerRunning Five) False False
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) players
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
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw cardInHand)) _ _ ->
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

                                                newGameStatus =
                                                    case Maybe.andThen Card.toPower cardToDiscard of
                                                        Just powerCard ->
                                                            BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BPlayerToPlay sessionId_ (BPlayerHasDiscard powerCard)) False False

                                                        Nothing ->
                                                            case nextPlayerSessionId maybeTamalouOwner sessionId players of
                                                                Just nextPlayerSessionId_ ->
                                                                    BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BPlayerToPlay nextPlayerSessionId_ (BWaitingPlayerAction Nothing)) False False

                                                                Nothing ->
                                                                    BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers (BEndTimerRunning Five) False False
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) updatedPlayers
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
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players bGameInProgressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
                                        let
                                            canTryToDouble =
                                                (maybeTamalouOwner /= Just sessionId)
                                                    && (case bGameInProgressStatus of
                                                            BStartTimerRunning _ ->
                                                                False

                                                            BPlayerToPlay sessionId_ bPlayerToPlayStatus ->
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
                                                                not isPlayerTurn || (isPlayerTurn && not hasPlayerDrawn)

                                                            BEndTimerRunning _ ->
                                                                True
                                                       )
                                        in
                                        if canTryToDouble then
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

                                                                newDiscardPile =
                                                                    { matchingCard | show = True } :: discardPile

                                                                orderedPlayers =
                                                                    players
                                                                        |> List.sortBy
                                                                            (\player ->
                                                                                Card.tableHandScore player.tableHand
                                                                            )
                                                                        |> showAllCardsOfAllPlayers

                                                                newGameStatus =
                                                                    if isGameFinished then
                                                                        BGameEnded orderedPlayers

                                                                    else
                                                                        BGameInProgress maybeTamalouOwner drawPile newDiscardPile updatedPlayers bGameInProgressStatus True canUsePowerFromLastPlayer
                                                            in
                                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) updatedPlayers
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
                                                                            BGameInProgress maybeTamalouOwner newDrawPile newDiscardPile updatedPlayers bGameInProgressStatus False canUsePowerFromLastPlayer
                                                                    in
                                                                    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) updatedPlayers
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

                    LookAtCardInTableHandToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerLookACard Nothing)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                        if sessionId == sessionId_ then
                                            let
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

                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile discardPile newPlayers (BPlayerToPlay sessionId_ (BPlayerLookACard (Just Two))) lastMoveIsDouble canUsePowerFromLastPlayer
                                            in
                                            ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) newPlayers
                                            )

                                        else
                                            ( { model | errors = "You are not allowed to look at a card 1 " :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                                    _ ->
                                        ( { model | errors = "You are not allowed to look at a card 2" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                            Nothing ->
                                ( { model | errors = "You are not allowed to look at a card 3" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                    TamalouToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress Nothing drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction _)) lastMoveIsDouble canUsePowerFromLastPlayer ->
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

                                                nextPlayerSessionId_ =
                                                    nextPlayerSessionId Nothing sessionId_ players |> Maybe.withDefault sessionId_
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
                                                                BGameInProgress Nothing newDrawPile newDiscardPile updatedPlayers (BPlayerToPlay nextPlayerSessionId_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble canUsePowerFromLastPlayer
                                                        in
                                                        ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
                                                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) updatedPlayers
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
                                                        BGameInProgress (Just sessionId) drawPile discardPile updatedPlayers (BPlayerToPlay nextPlayerSessionId_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble canUsePowerFromLastPlayer
                                                in
                                                ( { model | games = updateGameStatus urlPath ( newGameStatus, game.seed ) games }
                                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) updatedPlayers
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
                        -- 3) King is LookACard, which will result in the ability to look 1 card form the player hand for 5 seconds
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDiscard powerCard)) lastMoveIsDouble _ ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    case powerCard of
                                                        Card.PlayAgain ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble True

                                                        Card.Switch2Cards ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble True

                                                        Card.LookACard ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerLookACard Nothing)) lastMoveIsDouble True
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players

                                        else
                                            ( model, Cmd.none )

                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction (Just powerCard))) lastMoveIsDouble True ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    case powerCard of
                                                        Card.PlayAgain ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble False

                                                        Card.Switch2Cards ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble False

                                                        Card.LookACard ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerLookACard Nothing)) lastMoveIsDouble False
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    PowerIsNotUsedToBackend ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDiscard _)) lastMoveIsDouble _ ->
                                        if sessionId == sessionId_ then
                                            let
                                                newGameStatus =
                                                    case nextPlayerSessionId maybeTamalouOwner sessionId_ players of
                                                        Just nextPlayerSessionId_ ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay nextPlayerSessionId_ (BWaitingPlayerAction Nothing)) lastMoveIsDouble False

                                                        Nothing ->
                                                            BGameInProgress maybeTamalouOwner drawPile discardPile players (BEndTimerRunning Five) lastMoveIsDouble False
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    SendMessageToBackend newMessage ->
                        case maybeGame of
                            Just game ->
                                let
                                    playerName_ =
                                        playerName sessionId game

                                    newChat =
                                        game.chat ++ [ ( playerName_, newMessage ) ]

                                    newGame =
                                        { game | chat = newChat }
                                in
                                ( { model | games = updateGame newGame games }
                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateChatToFrontend newChat) (bPlayersFromFGame newGame)
                                )

                            Nothing ->
                                ( model, Cmd.none )


playerName : SessionId -> BGame -> String
playerName sessionId bGame =
    let
        players =
            bPlayersFromFGame bGame

        player =
            List.Extra.find ((==) sessionId << .sessionId) players
    in
    case player of
        Just p ->
            p.name

        Nothing ->
            "Unknown"


bPlayersFromFGame : BGame -> List BPlayer
bPlayersFromFGame game =
    case game.status of
        BWaitingForPlayers players ->
            players

        BGameInProgress _ _ _ players _ _ _ ->
            players

        BGameEnded orderedPlayers ->
            orderedPlayers


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
    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend <| toFGame (Just player.sessionId) newGameStatus) players
    )


nextPlayerSessionId : Maybe SessionId -> SessionId -> List BPlayer -> Maybe SessionId
nextPlayerSessionId maybeTamalouOwnerSessionId sessionId players =
    List.Extra.findIndex ((==) sessionId << .sessionId) players
        |> Maybe.andThen (\index_ -> List.Extra.getAt (modBy (List.length players) (index_ + 1)) players)
        |> Maybe.andThen
            (\p ->
                if maybeTamalouOwnerSessionId == Just p.sessionId then
                    Nothing

                else
                    Just p.sessionId
            )


toFGame : Maybe SessionId -> BGameStatus -> FGame
toFGame maybeSessionId backendGame =
    case backendGame of
        BWaitingForPlayers players ->
            FWaitingForPlayers (List.map (toFPlayer False) players)

        BGameInProgress Nothing bDrawPile discardPile players bGameInProgressStatus _ _ ->
            let
                ( tableHand, opponents ) =
                    findAndFilter ((==) maybeSessionId << Just << .sessionId) players
                        |> (\( maybeCurrentPlayer, opponents_ ) ->
                                case maybeCurrentPlayer of
                                    Just currentPlayer ->
                                        ( List.map toFCard currentPlayer.tableHand, List.map (toFPlayer False) opponents_ )

                                    Nothing ->
                                        ( [], List.map (toFPlayer False) opponents_ )
                           )
            in
            FGameInProgress Nothing tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile opponents (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameInProgress (Just tamalouOwnerSessionId) bDrawPile discardPile players bGameInProgressStatus _ _ ->
            let
                newPlayers =
                    showTamalouOwnerCards tamalouOwnerSessionId players

                ( tableHand, opponents ) =
                    newPlayers
                        |> findAndFilter ((==) maybeSessionId << Just << .sessionId)
                        |> (\( maybeCurrentPlayer, opponents_ ) ->
                                case maybeCurrentPlayer of
                                    Just currentPlayer ->
                                        ( if maybeSessionId == Just tamalouOwnerSessionId then
                                            currentPlayer.tableHand
                                                |> showAllCards
                                                |> List.map toFCard

                                          else
                                            List.map toFCard currentPlayer.tableHand
                                        , opponents_ |> List.map (toFPlayer False)
                                        )

                                    Nothing ->
                                        ( [], List.map (toFPlayer False) opponents_ )
                           )

                tamalouOwner =
                    List.Extra.find ((==) tamalouOwnerSessionId << .sessionId) players
                        |> Maybe.map (\p -> TamalouOwner p.sessionId p.tableHand)
            in
            FGameInProgress tamalouOwner tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile opponents (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameEnded orderedPlayers ->
            FGameEnded <| List.map (toFPlayer True) orderedPlayers


toFGameProgressStatus : Maybe SessionId -> BGameInProgressStatus -> FGameInProgressStatus
toFGameProgressStatus maybeSessionId bGameInProgressStatus =
    case bGameInProgressStatus of
        BStartTimerRunning timer ->
            FStartTimerRunning timer

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

                BPlayerLookACard maybeTimer ->
                    if maybeSessionId == Just sessionId then
                        FYourTurn (FPlayerLookACard maybeTimer)

                    else
                        FPlayerToPlay sessionId (FPlayerLookACard Nothing)

        BEndTimerRunning timer ->
            FEndTimerRunning timer


toFPlayer : Bool -> BPlayer -> FPlayer
toFPlayer sendScore bPlayer =
    { name = bPlayer.name
    , tableHand = List.map toFCard bPlayer.tableHand
    , clientId = bPlayer.clientId
    , sessionId = bPlayer.sessionId
    , ready = bPlayer.ready
    , score =
        if sendScore then
            Just <| Card.tableHandScore bPlayer.tableHand

        else
            Nothing
    }


toBPlayer : FPlayer -> BPlayer
toBPlayer fPlayer =
    { name = fPlayer.name
    , tableHand = []
    , clientId = fPlayer.clientId
    , sessionId = fPlayer.sessionId
    , ready = fPlayer.ready
    }


showTamalouOwnerCards : SessionId -> List BPlayer -> List BPlayer
showTamalouOwnerCards tamalouOwnerSessionId players =
    List.map
        (\player ->
            if player.sessionId == tamalouOwnerSessionId then
                { player | tableHand = List.map (\card -> { card | show = True }) player.tableHand }

            else
                player
        )
        players


toFCard : Card -> FCard
toFCard card =
    if card.show then
        FaceUp card

    else
        FaceDown


listOfFunnyPlaceHolderNames : List String
listOfFunnyPlaceHolderNames =
    [ "Francis Bacon"
    , "Benedict Egg"
    , "Taco Belle"
    , "Maxi Mom"
    , "Mini Mom"
    , "Sal A. Mander"
    , "Holly Wood"
    , "Alain Delonion"
    , "Jean DuJardinage"
    , "Samuel L. Jackfruit"
    , "Tom Ato"
    , "Framboise A. LaCreme"
    , "Jean-Claude Sans Dame"
    , "Tom C"
    , "Leonardo DiCarpaccio"
    , "Idris Elbarmesan"
    , "Marilyn Monrouleau"
    , "Jean-Paul Tartre"
    , "Angelina Jolie Haricot"
    , "Albert Einchampignon"
    ]


generateRandomFunnyName : Random.Seed -> List String -> ( String, Random.Seed )
generateRandomFunnyName seed alreadyNames =
    let
        filteredNames =
            List.filter (\name -> not (List.member name alreadyNames)) listOfFunnyPlaceHolderNames
    in
    Random.step (Random.int 0 (List.length filteredNames - 1)) seed
        |> (\( n, newSeed ) -> ( List.Extra.getAt n filteredNames |> Maybe.withDefault "Anonymous", newSeed ))


findAndFilter : (a -> Bool) -> List a -> ( Maybe a, List a )
findAndFilter predicate list =
    let
        found =
            List.filter predicate list |> List.head

        filtered =
            List.filter (\x -> not (predicate x)) list
    in
    ( found, filtered )


emptyBPlayer : SessionId -> ClientId -> BPlayer
emptyBPlayer sessionId clientId =
    { name = "Anonymous"
    , tableHand = []
    , clientId = clientId
    , sessionId = sessionId
    , ready = False
    }


showAllCards : List Card -> List Card
showAllCards =
    List.map (\card -> { card | show = True })
