module Backend exposing (app)

import Card
import Counter exposing (Counter(..))
import Game exposing (BGame, BGameInProgressStatus(..), BGameStatus(..), toFGame, updateGame)
import GameActionsToBackend exposing (handleActionFromGameToBackend)
import GameLogics exposing (assignRanks, nextPlayer)
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Player exposing (BPlayer, BPlayerToPlayStatus(..), LookACardStatus(..), Switch2CardsStatus(..), showAllCardsOfAllPlayers, stopDisplayCards)
import Random
import Task
import Time
import Types exposing (..)
import Utils.Random exposing (generateRandomFunnyName)


app : { init : ( BackendModel, Cmd BackendMsg ), subscriptions : BackendModel -> Sub BackendMsg, update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg ), updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg ) }
app =
    -- DebugApp.backend
    -- NoOpBackendMsg
    -- "e465a26049dfca11"
    Lamdera.backend
        { init = init
        , subscriptions = subscriptions
        , update = update
        , updateFromFrontend = updateFromFrontend
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { games = [], errors = [], admins = [] }
    , Cmd.none
    )


tickEverySecond : List BGame -> Sub BackendMsg
tickEverySecond games =
    games
        |> List.map
            (\g ->
                case g.status of
                    BGameInProgress _ _ _ _ (BStartTimerRunning _) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    BGameInProgress _ _ _ _ (BPlayerToPlay _ (BPlayerLookACard (LookingACard _ _))) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    BGameInProgress _ _ _ _ (BPlayerToPlay _ (BPlayerSwitch2Cards (OpponentCardChosen _ _ _))) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    BGameInProgress _ _ _ _ (BEndTimerRunning _) _ _ ->
                        Time.every 1000 (BackendMsgFromGame g.urlPath << TimerTick)

                    _ ->
                        Sub.none
            )
        |> Sub.batch


subscriptions : BackendModel -> Sub BackendMsg
subscriptions { games } =
    Sub.batch
        [ Lamdera.onDisconnect GotUserDisconnected
        , Lamdera.onConnect FeedSessionIdAndClientId
        , tickEverySecond games
        ]


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
                                            newPlayers : List BPlayer
                                            newPlayers =
                                                List.filter ((/=) sessionId << .sessionId) players
                                        in
                                        if List.length newPlayers /= List.length players then
                                            let
                                                newGameStatus : BGameStatus
                                                newGameStatus =
                                                    BWaitingForPlayers newPlayers
                                            in
                                            ( { game | status = newGameStatus }
                                            , List.map
                                                (\player -> Lamdera.sendToFrontend player.clientId (UpdateGameStatusToFrontend (toFGame Nothing newGameStatus) Nothing))
                                                newPlayers
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
            case toBackend of
                TimerTick _ ->
                    let
                        maybeGame : Maybe BGame
                        maybeGame =
                            List.Extra.find ((==) urlPath << .urlPath) games
                    in
                    case maybeGame of
                        Just game ->
                            case game.status of
                                BGameInProgress m a b (p1 :: restOfBPlayers) (BStartTimerRunning nb) _ _ ->
                                    let
                                        newGame : BGame
                                        newGame =
                                            { game
                                                | status =
                                                    case newNb of
                                                        Just newNb_ ->
                                                            BGameInProgress m a b (p1 :: restOfBPlayers) (BStartTimerRunning newNb_) False False

                                                        Nothing ->
                                                            BGameInProgress m a b (stopDisplayCards Nothing (p1 :: restOfBPlayers)) (BPlayerToPlay p1 (BWaitingPlayerAction Nothing)) False False
                                            }

                                        newNb : Maybe Counter
                                        newNb =
                                            Counter.decrement nb
                                    in
                                    ( { model | games = updateGame newGame games }
                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) (p1 :: restOfBPlayers)
                                    )

                                BGameInProgress maybeTamalouOwner a b (p1 :: restOfBPlayers) (BEndTimerRunning nb) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                    case Counter.decrement nb of
                                        Just nb_ ->
                                            let
                                                newGame : BGame
                                                newGame =
                                                    { game | status = BGameInProgress maybeTamalouOwner a b (p1 :: restOfBPlayers) (BEndTimerRunning nb_) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) (p1 :: restOfBPlayers)
                                            )

                                        Nothing ->
                                            let
                                                newGame : BGame
                                                newGame =
                                                    { game | status = BGameEnded orderedPlayers }

                                                orderedPlayers : List ( BPlayer, Int )
                                                orderedPlayers =
                                                    (p1 :: restOfBPlayers)
                                                        |> assignRanks maybeTamalouOwner
                                                        |> showAllCardsOfAllPlayers
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) (p1 :: restOfBPlayers)
                                            )

                                BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerLookACard (LookingACard index counter))) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                    case Counter.decrement counter of
                                        Just nb ->
                                            let
                                                newGame : BGame
                                                newGame =
                                                    { game | status = BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerLookACard (LookingACard index nb))) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
                                            )

                                        Nothing ->
                                            -- Debug to test the king power
                                            -- let
                                            --     newGame : BGame
                                            --     newGame =
                                            --         { game | status = BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerLookACard (LookingACard index One))) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            -- in
                                            -- ( { model | games = updateGame newGame games }
                                            -- , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
                                            -- )
                                            let
                                                maybeNextPlayer : Maybe BPlayer
                                                maybeNextPlayer =
                                                    nextPlayer maybeTamalouOwner bPlayer.sessionId players

                                                newGame : BGame
                                                newGame =
                                                    case maybeNextPlayer of
                                                        Just nextPlayer_ ->
                                                            { game
                                                                | status =
                                                                    BGameInProgress maybeTamalouOwner
                                                                        a
                                                                        b
                                                                        (stopDisplayCards Nothing players)
                                                                        (BPlayerToPlay nextPlayer_
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
                                                                    BGameInProgress maybeTamalouOwner a b (stopDisplayCards Nothing players) (BEndTimerRunning Five) lastMoveIsDouble canUsePowerFromLastPlayer
                                                            }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
                                            )

                                BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OpponentCardChosen ownCardIndex opponentCard counter))) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                    case Counter.decrement counter of
                                        Just nb ->
                                            let
                                                newGame : BGame
                                                newGame =
                                                    { game | status = BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OpponentCardChosen ownCardIndex opponentCard nb))) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
                                            )

                                        -- Debug to test the queen power
                                        -- Nothing ->
                                        --     let
                                        --         newGame : BGame
                                        --         newGame =
                                        --             { game | status = BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OpponentCardChosen ownCardIndex opponentCard One))) lastMoveIsDouble canUsePowerFromLastPlayer }
                                        --     in
                                        --     ( { model | games = updateGame newGame games }
                                        --     , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
                                        --     )
                                        Nothing ->
                                            let
                                                maybeNextPlayer : Maybe BPlayer
                                                maybeNextPlayer =
                                                    nextPlayer maybeTamalouOwner bPlayer.sessionId players

                                                newGame : BGame
                                                newGame =
                                                    case maybeNextPlayer of
                                                        Just nextPlayer_ ->
                                                            { game
                                                                | status =
                                                                    BGameInProgress maybeTamalouOwner
                                                                        a
                                                                        b
                                                                        (stopDisplayCards Nothing players)
                                                                        (BPlayerToPlay nextPlayer_
                                                                            (BWaitingPlayerAction
                                                                                (if canUsePowerFromLastPlayer then
                                                                                    Just Card.Switch2Cards

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
                                                                    BGameInProgress maybeTamalouOwner a b (stopDisplayCards Nothing players) (BEndTimerRunning Five) lastMoveIsDouble canUsePowerFromLastPlayer
                                                            }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) (Just <| AnimationSwitchCards ( bPlayer.sessionId, ownCardIndex ) ( opponentCard.sessionId, opponentCard.index ))) players
                                            )

                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                CreateGame posix clientId sessionId ->
                    let
                        ( funnyName, newSeed ) =
                            generateRandomFunnyName (Random.initialSeed (posix |> Time.posixToMillis)) []

                        newGame : BGame
                        newGame =
                            { urlPath = urlPath
                            , status = BWaitingForPlayers [ { name = funnyName, tableHand = [], clientId = clientId, sessionId = sessionId, ready = False } ]
                            , chat = []
                            , seed = newSeed
                            }
                    in
                    ( { model | games = newGame :: games }
                    , Lamdera.sendToFrontend clientId (UpdateGameStatusToFrontend (toFGame (Just sessionId) newGame.status) Nothing)
                    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg ({ games, errors } as model) =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        ActionFromGameToBackend urlPath toBackendActionFromGame ->
            if urlPath == "/reload" then
                init

            else
                case List.Extra.find ((==) urlPath << .urlPath) games of
                    Just game ->
                        handleActionFromGameToBackend model urlPath sessionId clientId game toBackendActionFromGame

                    Nothing ->
                        if toBackendActionFromGame == ConnectToBackend then
                            ( model
                            , Task.perform (\posix -> BackendMsgFromGame urlPath (CreateGame posix clientId sessionId)) Time.now
                            )

                        else
                            ( model, Cmd.none )

        ConnectToAdminToBackend ->
            ( model, Lamdera.sendToFrontend clientId (UpdateAdminToFrontend errors) )
