module Backend exposing (..)

import Card
import Html
import Lamdera exposing (ClientId, SessionId)
import Random
import Random.Extra as Random
import Random.List as Random
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
    Sub.batch [ Lamdera.onConnect GotUserConnected, Lamdera.onDisconnect GotUserDisconnected ]


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
                            players ++ [ { name = "Player " ++ String.fromInt (List.length players + 1), hand = [], clientId = clientId } ]

                        newGame =
                            BackendWaitingForPlayers newPlayers

                        frontendGame =
                            backendGameToFrontendGame newGame
                    in
                    case List.length newPlayers of
                        1 ->
                            ( { model | game = newGame }
                            , Cmd.batch [ Lamdera.sendToFrontend clientId (ConnectedBack clientId frontendGame), Lamdera.broadcast (UpdateGame frontendGame) ]
                            )

                        2 ->
                            ( { model | game = newGame }
                            , Cmd.batch [ Lamdera.sendToFrontend clientId (ConnectedBack clientId frontendGame), Lamdera.broadcast (UpdateGame frontendGame) ]
                            )

                        3 ->
                            ( { model | game = newGame }
                            , Cmd.batch [ Lamdera.sendToFrontend clientId (ConnectedBack clientId frontendGame), Lamdera.broadcast (UpdateGame frontendGame) ]
                            )

                        4 ->
                            ( { model | game = newGame }
                            , Cmd.batch
                                [ Lamdera.sendToFrontend clientId (ConnectedBack clientId frontendGame)
                                , Lamdera.broadcast (UpdateGame frontendGame)
                                , shuffleDrawPile
                                ]
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                BackendGameInProgress drawPile discardPile players ->
                    if List.member clientId (List.map .clientId players) then
                        let
                            frontendGame =
                                backendGameToFrontendGame model.game
                        in
                        ( model
                        , Cmd.batch [ Lamdera.sendToFrontend clientId (ConnectedBack clientId frontendGame), Lamdera.broadcast (UpdateGame frontendGame) ]
                        )

                    else
                        let
                            newModel =
                                { model | game = BackendGameInProgress drawPile discardPile (players ++ [ { name = "Player " ++ String.fromInt (List.length players + 1), hand = [], clientId = clientId } ]) }

                            frontendGame =
                                backendGameToFrontendGame newModel.game
                        in
                        ( newModel
                        , Cmd.batch [ Lamdera.sendToFrontend clientId (ConnectedBack clientId frontendGame), Lamdera.broadcast (UpdateGame frontendGame) ]
                        )

                BackendGameEnded _ ->
                    ( model
                    , Cmd.none
                    )

        GotUserDisconnected sessionId clientId ->
            let
                newModel =
                    case model.game of
                        BackendWaitingForPlayers players ->
                            { model | game = BackendWaitingForPlayers (List.filter (\player -> player.clientId /= clientId) players) }

                        BackendGameInProgress drawPile discardPile players ->
                            { model | game = BackendGameInProgress drawPile discardPile (List.filter (\player -> player.clientId /= clientId) players) }

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

                        BackendGameInProgress _ _ players_ ->
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
                    BackendGameInProgress newDrawPile discardPile newPlayers

                newModel =
                    { model | game = newGame }

                frontendGame =
                    backendGameToFrontendGame newGame
            in
            ( newModel, Lamdera.broadcast (UpdateGame frontendGame) )


distribute4CardsToPlayer : DrawPile -> BackendPlayer -> ( DrawPile, BackendPlayer )
distribute4CardsToPlayer drawPile player =
    case drawPile of
        [] ->
            ( drawPile, player )

        card1 :: [] ->
            ( [], { player | hand = player.hand ++ [ card1 ] } )

        card1 :: card2 :: [] ->
            ( [], { player | hand = player.hand ++ [ card1, card2 ] } )

        card1 :: card2 :: card3 :: [] ->
            ( [], { player | hand = player.hand ++ [ card1, card2, card3 ] } )

        card1 :: card2 :: card3 :: card4 :: drawPile_ ->
            ( drawPile_, { player | hand = player.hand ++ [ card1, card2, card3, card4 ] } )


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

        BackendGameInProgress drawPile discardPile players ->
            FrontendGameInProgress drawPile discardPile (List.map backendPlayerToFrontendPlayer players)

        BackendGameEnded clientId_ ->
            FrontendGameEnded clientId_


backendPlayerToFrontendPlayer : BackendPlayer -> FrontendPlayer
backendPlayerToFrontendPlayer backendPlayer =
    { name = backendPlayer.name
    , hand = backendPlayer.hand
    , clientId = backendPlayer.clientId
    }
