module Backend exposing (..)

import Card
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Random
import Random.Extra as Random
import Random.List as Random
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect GotUserConnected
        , Lamdera.onDisconnect GotUserDisconnected
        , launchTimer model.game
        ]


launchTimer : BackendGame -> Sub BackendMsg
launchTimer game =
    case game of
        BackendGameInProgress _ _ _ (TimerRunning _) ->
            Time.every 1000 TimerTick

        _ ->
            Sub.none


shuffleDrawPile : Cmd BackendMsg
shuffleDrawPile =
    Random.generate BeginGameAndDistribute4CardsToEach (Random.shuffle Card.nonShuffledDeck)


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { game = BackendWaitingForPlayers [] }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotUserConnected sessionId clientId ->
            case model.game of
                BackendWaitingForPlayers players ->
                    let
                        newPlayers =
                            case List.Extra.find ((==) sessionId << .sessionId) players of
                                Just _ ->
                                    players

                                Nothing ->
                                    players
                                        ++ [ { name = "Player " ++ String.fromInt (List.length players + 1)
                                             , hand = []
                                             , clientId = clientId
                                             , sessionId = sessionId
                                             }
                                           ]

                        newGame =
                            BackendWaitingForPlayers newPlayers

                        frontendGame =
                            backendGameToFrontendGame newGame
                    in
                    case List.length newPlayers of
                        1 ->
                            ( { model | game = newGame }
                            , Cmd.batch
                                [ Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId frontendGame)
                                , Lamdera.broadcast (UpdateGame frontendGame)
                                ]
                            )

                        2 ->
                            ( { model | game = newGame }
                            , Cmd.batch
                                [ Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId frontendGame)
                                , Lamdera.broadcast (UpdateGame frontendGame)
                                ]
                            )

                        3 ->
                            ( { model | game = newGame }
                            , Cmd.batch
                                [ Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId frontendGame)
                                , Lamdera.broadcast (UpdateGame frontendGame)
                                ]
                            )

                        4 ->
                            ( { model | game = newGame }
                            , Cmd.batch
                                [ Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId frontendGame)
                                , shuffleDrawPile
                                ]
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                BackendGameInProgress drawPile discardPile players maybeTimer ->
                    case List.Extra.find ((==) sessionId << .sessionId) players of
                        Just _ ->
                            let
                                frontendGame =
                                    backendGameToFrontendGame model.game
                            in
                            ( model
                            , Cmd.batch
                                [ Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId frontendGame)
                                ]
                            )

                        Nothing ->
                            ( model, Cmd.none )

                BackendGameEnded _ ->
                    ( model
                    , Cmd.none
                    )

        GotUserDisconnected sessionId clientId ->
            let
                newModel =
                    case model.game of
                        BackendWaitingForPlayers players ->
                            { model | game = BackendWaitingForPlayers (List.filter ((/=) sessionId << .sessionId) players) }

                        BackendGameInProgress drawPile discardPile players maybeTimer ->
                            model

                        BackendGameEnded _ ->
                            model
            in
            ( newModel
            , Lamdera.broadcast (UpdateGame <| backendGameToFrontendGame newModel.game)
            )

        -- List.drop 4 drawPile for each player
        BeginGameAndDistribute4CardsToEach shuffledDeck ->
            let
                players =
                    case model.game of
                        BackendWaitingForPlayers players_ ->
                            players_

                        BackendGameInProgress _ _ players_ maybeTimer ->
                            players_

                        BackendGameEnded _ ->
                            []

                discardPile =
                    []

                drawPile =
                    shuffledDeck

                ( newDrawPile, newPlayers ) =
                    List.foldl
                        (\player ( drawPile_, players_ ) ->
                            let
                                ( newDrawPile_, newPlayer ) =
                                    distribute4CardsToPlayer drawPile_ player
                            in
                            ( newDrawPile_, players_ ++ [ newPlayer ] )
                        )
                        ( drawPile, [] )
                        players

                newGame =
                    BackendGameInProgress newDrawPile discardPile newPlayers (TimerRunning 5)

                newModel =
                    { model | game = newGame }

                frontendGame =
                    backendGameToFrontendGame newGame
            in
            ( newModel, Cmd.batch [ Lamdera.broadcast (UpdateGame frontendGame) ] )

        TimerTick _ ->
            let
                newGame =
                    case model.game of
                        BackendGameInProgress a b (p1 :: players) (TimerRunning nb) ->
                            if (nb - 1) < 0 then
                                let
                                    newPlayers =
                                        stopDisplayCards (p1 :: players)
                                in
                                BackendGameInProgress a b newPlayers (PlayerToPlay p1.sessionId)

                            else
                                BackendGameInProgress a b (p1 :: players) (TimerRunning <| nb - 1)

                        _ ->
                            model.game
            in
            ( { model | game = newGame }, Lamdera.broadcast (UpdateGame <| backendGameToFrontendGame newGame) )


stopDisplayCards : List BackendPlayer -> List BackendPlayer
stopDisplayCards players =
    List.map
        (\({ hand } as player) ->
            { player | hand = List.map (\card -> { card | show = False }) hand }
        )
        players


distribute4CardsToPlayer : DrawPile -> BackendPlayer -> ( DrawPile, BackendPlayer )
distribute4CardsToPlayer drawPile player =
    case drawPile of
        [] ->
            ( drawPile, player )

        card1 :: [] ->
            ( [], { player | hand = [ { card1 | show = True } ] } )

        card1 :: card2 :: [] ->
            ( [], { player | hand = [ { card1 | show = True }, card2 ] } )

        card1 :: card2 :: card3 :: [] ->
            ( [], { player | hand = [ { card1 | show = True }, card2, card3 ] } )

        card1 :: card2 :: card3 :: card4 :: drawPile_ ->
            ( drawPile_, { player | hand = [ { card1 | show = True }, card2, card3, { card4 | show = True } ] } )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )


backendGameToFrontendGame : BackendGame -> FrontendGame
backendGameToFrontendGame backendGame =
    case backendGame of
        BackendWaitingForPlayers players ->
            FrontendWaitingForPlayers (List.map backendPlayerToFrontendPlayer players)

        BackendGameInProgress drawPile discardPile players maybeTimer ->
            FrontendGameInProgress drawPile discardPile (List.map backendPlayerToFrontendPlayer players) maybeTimer

        BackendGameEnded clientId_ ->
            FrontendGameEnded clientId_


backendPlayerToFrontendPlayer : BackendPlayer -> FrontendPlayer
backendPlayerToFrontendPlayer backendPlayer =
    { name = backendPlayer.name
    , hand = backendPlayer.hand
    , clientId = backendPlayer.clientId
    , sessionId = backendPlayer.sessionId
    }
