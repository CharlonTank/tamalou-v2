module Backend exposing (..)

import Card exposing (Card, FCard(..))
import DebugApp
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Maybe.Extra as Maybe
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
        BGameInProgress _ _ _ (BTimerRunning _) _ ->
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
                                             , tableHand = []
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

                BGameInProgress drawPile discardPile players maybeTimer lastMoveIsDouble ->
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
                                    BGameInProgress drawPile discardPile updateClientIdInPlayers maybeTimer lastMoveIsDouble
                            in
                            ( { model | game = newGame }
                              -- let's use backendGameToFrontendGame on each player
                            , Cmd.batch <|
                                Lamdera.sendToFrontend clientId (ConnectedBack sessionId clientId (backendGameToFrontendGame (Just sessionId) newGame))
                                    :: List.map (\p -> Lamdera.sendToFrontend p.clientId <| UpdateGame <| backendGameToFrontendGame (Just p.sessionId) newGame) updateClientIdInPlayers
                            )

                        Nothing ->
                            ( model, Lamdera.sendToFrontend clientId (UpdateGame FGameAlreadyStartedWithoutYou) )

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

                BGameInProgress drawPile discardPile players maybeTimer _ ->
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

                        BGameInProgress _ _ players_ maybeTimer _ ->
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
                    BGameInProgress newDrawPile discardPile newPlayers (BTimerRunning 5) False

                newModel =
                    { model | game = newGame }
            in
            ( newModel
            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) newPlayers
            )

        TimerTick _ ->
            case game of
                BGameInProgress a b (p1 :: restOfPlayers) (BTimerRunning nb) lastMoveIsDouble ->
                    let
                        newGame =
                            if (nb - 1) < 0 then
                                let
                                    newPlayers =
                                        stopDisplayCards (p1 :: restOfPlayers)
                                in
                                BGameInProgress a b newPlayers (BPlayerToPlay p1.sessionId BWaitingPlayerDraw) lastMoveIsDouble

                            else
                                BGameInProgress a b (p1 :: restOfPlayers) (BTimerRunning <| nb - 1) lastMoveIsDouble
                    in
                    ( { model | game = newGame }
                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) (p1 :: restOfPlayers)
                    )

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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg ({ game } as model) =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        DrawCardFromDrawPileToBackend ->
            case game of
                BGameInProgress drawPile discardPile players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) _ ->
                    if sessionId == sessionId_ then
                        ( model, Cmd.none )
                            |> drawInDrawPile sessionId

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DrawCardFromDiscardPileToBackend ->
            case game of
                BGameInProgress drawPile (head :: rest) players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) _ ->
                    if sessionId == sessionId_ then
                        let
                            newGame =
                                BGameInProgress drawPile rest players (BPlayerToPlay sessionId_ (BPlayerHasDraw head)) False
                        in
                        ( { model | game = newGame }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) players
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DiscardCardInHandToBackend ->
            case game of
                BGameInProgress drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw card)) _ ->
                    if sessionId == sessionId_ then
                        let
                            newGame =
                                BGameInProgress drawPile (card :: discardPile) players (BPlayerToPlay (nextPlayerSessionId sessionId_ players) BWaitingPlayerDraw) False
                        in
                        ( { model | game = newGame }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) players
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReplaceCardInTableHandToBackend cardIndex ->
            case game of
                BGameInProgress drawPile discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw cardInHand)) _ ->
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

                            newDiscardPile =
                                currentPlayer |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand) |> Maybe.map (\c -> { c | show = True }) |> Maybe.map (\c -> c :: discardPile) |> Maybe.withDefault discardPile

                            newGame =
                                BGameInProgress drawPile newDiscardPile updatedPlayers (BPlayerToPlay (nextPlayerSessionId sessionId_ players) BWaitingPlayerDraw) False
                        in
                        ( { model | game = newGame }
                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) updatedPlayers
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- For DoubleCardInTableHandToBackend action, the card is just removed for tableHand of the player, and added to the discardPile, only if the last card in the discardPile has the same Rank. But this action can only be done one time per player turn but by anyone. If someone tries to triple, this player gets his card back and gets a penalty of 1 card from the drawPile.
        DoubleCardInTableHandToBackend cardIndex ->
            case game of
                BGameInProgress drawPile discardPile players (BPlayerToPlay sessionId_ bPlayerToPlayStatus) doubleIsLastMove ->
                    -- Determine if it's currently the player's turn and if they've drawn a card
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
                        -- Check if there's at least one card in the discardPile to match against
                        case List.Extra.uncons discardPile of
                            Just ( dicardPileHead, _ ) ->
                                let
                                    currentPlayer =
                                        List.Extra.find ((==) sessionId << .sessionId) players

                                    -- Attempt to find the card in the current player's table hand and ensure it matches the last card in the discard pile
                                    matchingCard =
                                        currentPlayer
                                            |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)
                                            |> Maybe.andThen
                                                (\card ->
                                                    if card.rank == dicardPileHead.rank then
                                                        Just card

                                                    else
                                                        Nothing
                                                )
                                in
                                case ( doubleIsLastMove, matchingCard ) of
                                    ( False, Just _ ) ->
                                        let
                                            updatedPlayers =
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

                                            newDiscardPile =
                                                matchingCard
                                                    |> Maybe.map (\card -> { card | show = True } :: discardPile)
                                                    |> Maybe.withDefault discardPile

                                            newGame =
                                                BGameInProgress drawPile newDiscardPile updatedPlayers (BPlayerToPlay sessionId_ bPlayerToPlayStatus) True
                                        in
                                        ( { model | game = newGame }
                                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) (BGameInProgress drawPile newDiscardPile updatedPlayers (BPlayerToPlay sessionId_ bPlayerToPlayStatus) True)) updatedPlayers
                                        )

                                    _ ->
                                        -- The card doesn't match or someone already doubled before; penalize the player attempting the double move if necessary by drawing from the drawPile one card and adding it directly to the tableHand
                                        -- to do that, we can just check if there is a card in the drawPile, if not, for now, we can Debug.todo "PLUS DE CARTES DANS LA DRAW PILE", and if there is, we can just draw it and add it to the tableHand of the player and update the drawPile
                                        let
                                            ( cardPenalty, newDrawPile ) =
                                                case List.Extra.uncons drawPile of
                                                    Just ( head, rest ) ->
                                                        ( head, rest )

                                                    Nothing ->
                                                        Debug.todo "PLUS DE CARTES DANS LA DRAW PILE"

                                            newPlayers =
                                                players
                                                    |> List.map
                                                        (\p ->
                                                            if p.sessionId == sessionId then
                                                                { p
                                                                    | tableHand =
                                                                        p.tableHand
                                                                            ++ [ { cardPenalty | show = False } ]
                                                                }

                                                            else
                                                                p
                                                        )

                                            newGame =
                                                BGameInProgress newDrawPile discardPile newPlayers (BPlayerToPlay sessionId_ BWaitingPlayerDraw) doubleIsLastMove
                                        in
                                        ( { model | game = newGame }
                                        , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGame <| backendGameToFrontendGame (Just player.sessionId) newGame) newPlayers
                                        )

                            _ ->
                                -- There are no cards in the discardPile to match against
                                ( model, Cmd.none )

                    else
                        -- It's the player's turn, and they've drawn a card; doubling is not allowed
                        ( model, Cmd.none )

                _ ->
                    -- If the game is not in progress or in an unexpected state, no action is performed
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
        BGameInProgress [] discardPile players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) _ ->
            if sessionId == sessionId_ then
                ( model, Cmd.none )
                    |> Debug.todo "PLUS DE CARTES DANS LA DRAW PILE"
                -- |> drawInDrawPile sessionId

            else
                ( model, cmds )

        BGameInProgress (head :: rest) discardPile players (BPlayerToPlay sessionId_ BWaitingPlayerDraw) lastMoveIsDouble ->
            if sessionId == sessionId_ then
                let
                    newGame =
                        BGameInProgress rest discardPile players (BPlayerToPlay sessionId_ (BPlayerHasDraw head)) lastMoveIsDouble
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

        BGameInProgress bDrawPile discardPile players bGameInProgressStatus _ ->
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
            FGameInProgress tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile (List.map backendPlayerToFrontendPlayer players) (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

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
                    if maybeSessionId == Just sessionId then
                        FYourTurn FWaitingPlayerDraw

                    else
                        FPlayerToPlay sessionId FWaitingPlayerDraw

                BPlayerHasDraw card ->
                    if maybeSessionId == Just sessionId then
                        FYourTurn (FPlayerHasDraw (FaceUp card))

                    else
                        FPlayerToPlay sessionId (FPlayerHasDraw FaceDown)


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
