module Backend exposing (..)

import Card exposing (Card, FCard(..))
import DebugApp
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Random
import Random.Extra as Random
import Random.List as Random
import Time
import Types exposing (..)


app =
    DebugApp.backend
        NoOpBackendMsg
        "8c7b7b0c1fc0357e"
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions { game } =
    Sub.batch
        [ Lamdera.onDisconnect GotUserDisconnected
        , Lamdera.onConnect GotUserConnected
        , launchTimer game
        ]


launchTimer : BGame -> Sub BackendMsg
launchTimer game =
    case game of
        BGameInProgress _ _ _ (BTimerRunning _) ->
            Time.every 1000 TimerTick

        _ ->
            Sub.none


shuffleDrawPile : Cmd BackendMsg
shuffleDrawPile =
    Random.generate BeginGameAndDistribute4CardsToEach (Random.shuffle Card.nonShuffledDeck)


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { game = BWaitingForPlayers [] }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg ({ game } as model) =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotUserConnected sessionId clientId ->
            case game of
                BWaitingForPlayers players ->
                    let
                        newPlayers =
                            case List.Extra.find (\p -> p.sessionId == sessionId) players of
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
                            BWaitingForPlayers newPlayers

                        frontendGame : FGame
                        frontendGame =
                            backendGameToFrontendGame Nothing newGame
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

                BGameInProgress drawPile discardPile players maybeTimer ->
                    -- case List.Extra.find ((==) sessionId << .sessionId) players of
                    --     Just _ ->
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
                            BGameInProgress drawPile discardPile updateClientIdInPlayers maybeTimer

                        -- frontendGame =
                        --     backendGameToFrontendGame (Just player.sessionId) newGame
                    in
                    ( { model | game = newGame }
                      -- let's use backendGameToFrontendGame on each player
                    , Cmd.batch <|
                        Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId (backendGameToFrontendGame (Just sessionId) newGame))
                            :: List.map (\p -> Lamdera.sendToFrontend p.clientId <| UpdateGame <| backendGameToFrontendGame (Just p.sessionId) newGame) updateClientIdInPlayers
                    )

                -- Nothing ->
                --     ( model, Cmd.none )
                BGameEnded _ ->
                    ( model
                    , Cmd.none
                    )

        GotUserDisconnected sessionId clientId ->
            case game of
                BWaitingForPlayers players ->
                    let
                        newGame =
                            BWaitingForPlayers (List.filter ((/=) sessionId << .sessionId) players)
                    in
                    ( { model | game = newGame }
                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) players
                    )

                BGameInProgress drawPile discardPile players maybeTimer ->
                    ( model, Cmd.none )

                BGameEnded _ ->
                    ( model, Cmd.none )

        -- List.drop 4 drawPile for each player
        BeginGameAndDistribute4CardsToEach shuffledDeck ->
            let
                players =
                    case game of
                        BWaitingForPlayers players_ ->
                            players_

                        BGameInProgress _ _ players_ maybeTimer ->
                            players_

                        BGameEnded _ ->
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
                    BGameInProgress newDrawPile discardPile newPlayers (BTimerRunning 5)

                newModel =
                    { model | game = newGame }
            in
            ( newModel
            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) newPlayers
            )

        TimerTick _ ->
            case game of
                BGameInProgress a b (p1 :: restOfPlayers) (BTimerRunning nb) ->
                    let
                        newGame =
                            if (nb - 1) < 0 then
                                let
                                    newPlayers =
                                        stopDisplayCards (p1 :: restOfPlayers)
                                in
                                BGameInProgress a b newPlayers (BPlayerToPlay p1.sessionId BWaitingPlayerDraw)

                            else
                                BGameInProgress a b (p1 :: restOfPlayers) (BTimerRunning <| nb - 1)
                    in
                    ( { model | game = newGame }
                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) (p1 :: restOfPlayers)
                    )

                _ ->
                    ( model, Cmd.none )


stopDisplayCards : List BPlayer -> List BPlayer
stopDisplayCards players =
    List.map
        (\({ hand } as player) ->
            { player | hand = List.map (\card -> { card | show = False }) hand }
        )
        players


distribute4CardsToPlayer : BDrawPile -> BPlayer -> ( BDrawPile, BPlayer )
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
updateFromFrontend sessionId clientId msg ({ game } as model) =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        DrawCardFromDrawPileToBackend ->
            case game of
                BGameInProgress drawPile discardPile players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) ->
                    if sessionId == sessionId_ then
                        ( model, Cmd.none )
                            |> drawInDrawPile sessionId

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DiscardCardToBackend ->
            case game of
                BGameInProgress drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw card)) ->
                    if sessionId == sessionId_ then
                        let
                            newGame =
                                BGameInProgress drawPile (card :: discardPile) players (BPlayerToPlay (nextPlayerSessionId sessionId_ players) BWaitingPlayerDraw)
                        in
                        ( { model | game = newGame }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) players
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


nextPlayerSessionId : SessionId -> List BPlayer -> SessionId
nextPlayerSessionId sessionId players =
    List.Extra.findIndex ((==) sessionId << .sessionId) players
        |> Maybe.andThen (\index_ -> List.Extra.getAt (modBy (List.length players) (index_ + 1)) players)
        |> Maybe.map .sessionId
        |> Maybe.withDefault sessionId


drawInDrawPile : SessionId -> ( BackendModel, Cmd BackendMsg ) -> ( BackendModel, Cmd BackendMsg )
drawInDrawPile sessionId ( { game } as model, cmds ) =
    case game of
        BGameInProgress [] discardPile players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) ->
            if sessionId == sessionId_ then
                ( model, Cmd.none )
                    |> Debug.todo "PLUS DE CARTES DANS LA DRAW PILE"
                -- |> drawInDrawPile sessionId

            else
                ( model, cmds )

        BGameInProgress (head :: rest) discardPile players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) ->
            if sessionId == sessionId_ then
                let
                    newGame =
                        BGameInProgress rest discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw head))
                in
                ( { model | game = newGame }
                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) players
                )

            else
                ( model, cmds )

        _ ->
            ( model, cmds )


backendGameToFrontendGame : Maybe SessionId -> BGame -> FGame
backendGameToFrontendGame maybeSessionId backendGame =
    case backendGame of
        BWaitingForPlayers players ->
            FWaitingForPlayers (List.map backendPlayerToFrontendPlayer players)

        BGameInProgress bDrawPile discardPile players bGameInProgressStatus ->
            let
                hand =
                    case maybeSessionId of
                        Just sessionId ->
                            List.Extra.find ((==) sessionId << .sessionId) players
                                |> Maybe.map .hand
                                |> Maybe.map (List.map cardToFrontendCard)
                                |> Maybe.withDefault [ FaceUp Card.sampleCard, FaceUp Card.sampleCard ]

                        Nothing ->
                            [ FaceUp Card.sampleCard ]
            in
            FGameInProgress hand (List.map (always Card.FaceDown) bDrawPile) discardPile (List.map backendPlayerToFrontendPlayer players) (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameEnded clientId_ ->
            FGameEnded clientId_


toFGameProgressStatus : Maybe SessionId -> BGameInProgressStatus -> FGameInProgressStatus
toFGameProgressStatus maybeSessionId bGameInProgressStatus =
    case bGameInProgressStatus of
        BTimerRunning timer ->
            FTimerRunning timer

        BPlayerToPlay sessionId bPlayerToPlayStatus ->
            case bPlayerToPlayStatus of
                BWaitingPlayerDraw ->
                    FPlayerToPlay sessionId FWaitingPlayerDraw

                BPlayerHasDraw card ->
                    FPlayerToPlay sessionId
                        (FPlayerHasDraw
                            (if maybeSessionId == Just sessionId then
                                FaceUp card

                             else
                                FaceDown
                            )
                        )


backendPlayerToFrontendPlayer : BPlayer -> FPlayer
backendPlayerToFrontendPlayer backendPlayer =
    { name = backendPlayer.name
    , hand = List.map cardToFrontendCard backendPlayer.hand
    , clientId = backendPlayer.clientId
    , sessionId = backendPlayer.sessionId
    }


cardToFrontendCard : Card -> FCard
cardToFrontendCard card =
    if card.show then
        FaceUp card

    else
        FaceDown
