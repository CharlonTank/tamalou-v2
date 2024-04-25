module Backend exposing (app)

import Card exposing (Card, FCard(..), handIsLessThanFive)
import DebugApp
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Random
import Task
import Time
import Types exposing (..)


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


bPlayersFromFGame : BGame -> List BPlayer
bPlayersFromFGame game =
    case game.status of
        BWaitingForPlayers players ->
            players

        BGameInProgress _ _ _ players _ _ _ ->
            players

        BGameEnded orderedPlayers ->
            orderedPlayers |> List.map Tuple.first


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
            Nothing


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


emptyBPlayer : SessionId -> ClientId -> BPlayer
emptyBPlayer sessionId clientId =
    { name = "Anonymous"
    , tableHand = []
    , clientId = clientId
    , sessionId = sessionId
    , ready = False
    }


findAndRearrange : (a -> Bool) -> List a -> ( Maybe a, List a )
findAndRearrange predicate list =
    case List.Extra.findIndex predicate list of
        Just index ->
            let
                after : List a
                after =
                    List.drop 1 foundAndAfter

                ( before, foundAndAfter ) =
                    List.Extra.splitAt index list

                found : Maybe a
                found =
                    List.head foundAndAfter
            in
            ( found, after ++ before )

        Nothing ->
            ( Nothing, list )


generateRandomFunnyName : Random.Seed -> List String -> ( String, Random.Seed )
generateRandomFunnyName seed alreadyNames =
    let
        filteredNames : List String
        filteredNames =
            List.filter (\name -> not (List.member name alreadyNames)) listOfFunnyPlaceHolderNames
    in
    Random.step (Random.int 0 (List.length filteredNames - 1)) seed
        |> (\( n, newSeed ) -> ( List.Extra.getAt n filteredNames |> Maybe.withDefault "Anonymous", newSeed ))


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { games = [], errors = [], admins = [] }
    , Cmd.none
    )


launchTimer : List BGame -> Sub BackendMsg
launchTimer games =
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


nextPlayer : Maybe SessionId -> SessionId -> List BPlayer -> Maybe BPlayer
nextPlayer maybeTamalouOwnerSessionId sessionId players =
    List.Extra.findIndex ((==) sessionId << .sessionId) players
        |> Maybe.andThen (\index_ -> List.Extra.getAt (modBy (List.length players) (index_ + 1)) players)
        |> Maybe.andThen
            (\p ->
                if maybeTamalouOwnerSessionId == Just p.sessionId then
                    Nothing

                else
                    Just p
            )


playerName : SessionId -> BGame -> String
playerName sessionId bGame =
    let
        player : Maybe BPlayer
        player =
            List.Extra.find ((==) sessionId << .sessionId) players

        players : List BPlayer
        players =
            bPlayersFromFGame bGame
    in
    case player of
        Just p ->
            p.name

        Nothing ->
            "Unknown"


showAllCards : List Card -> List Card
showAllCards =
    List.map (\card -> { card | show = True })


showAllCardsOfAllPlayers : List ( BPlayer, Int ) -> List ( BPlayer, Int )
showAllCardsOfAllPlayers players =
    List.map
        (\( { tableHand } as player, r ) ->
            ( { player | tableHand = List.map (\card -> { card | show = True }) tableHand }, r )
        )
        players


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

        shuffledList : List a
        shuffledList =
            taggedList
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
    in
    ( shuffledList, finalSeed )


stopDisplayCards : Maybe SessionId -> List BPlayer -> List BPlayer
stopDisplayCards tamalouOwnerSessionId players =
    case tamalouOwnerSessionId of
        Just ownerId ->
            List.map
                (\player ->
                    if player.sessionId == ownerId then
                        player

                    else
                        { player | tableHand = List.map (\card -> { card | show = False }) player.tableHand }
                )
                players

        Nothing ->
            List.map
                (\player ->
                    { player | tableHand = List.map (\card -> { card | show = False }) player.tableHand }
                )
                players


subscriptions : BackendModel -> Sub BackendMsg
subscriptions { games } =
    Sub.batch
        [ Lamdera.onDisconnect GotUserDisconnected
        , Lamdera.onConnect FeedSessionIdAndClientId
        , launchTimer games
        ]


toBPlayer : FPlayer -> BPlayer
toBPlayer fPlayer =
    { name = fPlayer.name
    , tableHand = []
    , clientId = fPlayer.clientId
    , sessionId = fPlayer.sessionId
    , ready = fPlayer.ready
    }


toFCard : Card -> FCard
toFCard card =
    if card.show then
        FaceUp card

    else
        FaceDown


toFGame : Maybe SessionId -> BGameStatus -> FGame
toFGame maybeSessionId backendGame =
    case backendGame of
        BWaitingForPlayers players ->
            FWaitingForPlayers (List.map (toFPlayer False) players)

        BGameInProgress (Just tamalouOwnerSessionId) bDrawPile discardPile players bGameInProgressStatus _ _ ->
            let
                newPlayers : List BPlayer
                newPlayers =
                    showTamalouOwnerCards tamalouOwnerSessionId players

                ( tableHand, opponents ) =
                    newPlayers
                        |> findAndRearrange ((==) maybeSessionId << Just << .sessionId)
                        |> (\( maybeCurrentPlayer, opponents_ ) ->
                                case maybeCurrentPlayer of
                                    Just currentPlayer ->
                                        ( if maybeSessionId == Just tamalouOwnerSessionId then
                                            currentPlayer.tableHand
                                                |> showAllCards
                                                |> List.map toFCard

                                          else
                                            List.map toFCard currentPlayer.tableHand
                                        , opponents_ |> stopDisplayCards (Just tamalouOwnerSessionId) |> List.map (toFPlayer False)
                                        )

                                    Nothing ->
                                        ( [], opponents_ |> stopDisplayCards (Just tamalouOwnerSessionId) |> List.map (toFPlayer False) )
                           )

                tamalouOwner : Maybe TamalouOwner
                tamalouOwner =
                    List.Extra.find ((==) tamalouOwnerSessionId << .sessionId) players
                        |> Maybe.map (\p -> TamalouOwner p.sessionId p.tableHand)
            in
            FGameInProgress tamalouOwner tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile opponents (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameInProgress Nothing bDrawPile discardPile players bGameInProgressStatus _ _ ->
            let
                ( tableHand, opponents ) =
                    findAndRearrange ((==) maybeSessionId << Just << .sessionId) players
                        |> (\( maybeCurrentPlayer, opponents_ ) ->
                                case maybeCurrentPlayer of
                                    Just currentPlayer ->
                                        ( List.map toFCard currentPlayer.tableHand, opponents_ |> stopDisplayCards Nothing |> List.map (toFPlayer False) )

                                    Nothing ->
                                        ( [], opponents_ |> stopDisplayCards Nothing |> List.map (toFPlayer False) )
                           )
            in
            FGameInProgress Nothing tableHand (List.map (always Card.FaceDown) bDrawPile) discardPile opponents (toFGameProgressStatus maybeSessionId bGameInProgressStatus)

        BGameEnded orderedPlayers ->
            FGameEnded <| List.map (\( p, r ) -> ( toFPlayer True p, r )) orderedPlayers


toFGameProgressStatus : Maybe SessionId -> BGameInProgressStatus -> FGameInProgressStatus
toFGameProgressStatus maybeSessionId bGameInProgressStatus =
    case bGameInProgressStatus of
        BStartTimerRunning timer ->
            FStartTimerRunning timer

        BPlayerToPlay bPlayer bPlayerToPlayStatus ->
            case bPlayerToPlayStatus of
                BWaitingPlayerAction maybePowerCard ->
                    if maybeSessionId == Just bPlayer.sessionId then
                        FYourTurn (FWaitingPlayerAction maybePowerCard)

                    else
                        FPlayerToPlay (toFPlayer False bPlayer) (FWaitingPlayerAction maybePowerCard)

                BPlayerHasDrawn card ->
                    if maybeSessionId == Just bPlayer.sessionId then
                        FYourTurn (FPlayerHasDraw (FaceUp card))

                    else
                        FPlayerToPlay (toFPlayer False bPlayer) (FPlayerHasDraw FaceDown)

                BPlayerHasDiscard powerCard ->
                    if maybeSessionId == Just bPlayer.sessionId then
                        FYourTurn (FPlayerHasDiscard powerCard)

                    else
                        FPlayerToPlay (toFPlayer False bPlayer) (FPlayerHasDiscard powerCard)

                BPlayerLookACard lookAtCardStatus ->
                    if maybeSessionId == Just bPlayer.sessionId then
                        FYourTurn (FPlayerLookACard lookAtCardStatus)

                    else
                        FPlayerToPlay (toFPlayer False bPlayer) (FPlayerLookACard lookAtCardStatus)

                BPlayerSwitch2Cards switch2CardsStatus ->
                    if maybeSessionId == Just bPlayer.sessionId then
                        FYourTurn (FPlayerSwitch2Cards switch2CardsStatus)

                    else
                        FPlayerToPlay (toFPlayer False bPlayer) (FPlayerSwitch2Cards switch2CardsStatus)

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
                                            decrementCounter nb
                                    in
                                    ( { model | games = updateGame newGame games }
                                    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) (p1 :: restOfBPlayers)
                                    )

                                BGameInProgress maybeTamalouOwner a b (p1 :: restOfBPlayers) (BEndTimerRunning nb) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                    case decrementCounter nb of
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
                                    case decrementCounter counter of
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
                                    case decrementCounter counter of
                                        Just nb ->
                                            let
                                                newGame : BGame
                                                newGame =
                                                    { game | status = BGameInProgress maybeTamalouOwner a b players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OpponentCardChosen ownCardIndex opponentCard nb))) lastMoveIsDouble canUsePowerFromLastPlayer }
                                            in
                                            ( { model | games = updateGame newGame games }
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
                                            )

                                        -- Test Switch, to be removed once we're sure it works everytime
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
                                            , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGame.status) Nothing) players
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
                let
                    maybeGame : Maybe BGame
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

                            Nothing ->
                                ( model, Cmd.none )

                    ImReadyToBackend ->
                        case maybeGame of
                            Just game ->
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
                                                    shuffleWithSeed game.seed (Debug.log "WARNING" Card.debugDeck)

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

                            Nothing ->
                                ( model, Cmd.none )

                    ReStartGameToBackend maybeFPlayer ->
                        case maybeGame of
                            Just game ->
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

                            Nothing ->
                                ( model, Cmd.none )

                    DrawFromDrawPileToBackend ->
                        case maybeGame of
                            Just game ->
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

                            Nothing ->
                                ( model, Cmd.none )

                    DiscardCardInHandToBackend ->
                        case maybeGame of
                            Just game ->
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

                            Nothing ->
                                ( model, Cmd.none )

                    DrawFromDiscardPileToBackend ->
                        case maybeGame of
                            Just game ->
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

                            Nothing ->
                                ( model, Cmd.none )

                    ReplaceCardInTableHandToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerHasDrawn cardInHand)) _ _ ->
                                        if sessionId == bPlayer.sessionId then
                                            let
                                                maybeCurrentPlayer : Maybe BPlayer
                                                maybeCurrentPlayer =
                                                    List.Extra.find ((==) sessionId << .sessionId) players

                                                maybeCardToDiscard : Maybe Card
                                                maybeCardToDiscard =
                                                    maybeCurrentPlayer
                                                        |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)
                                                        |> Maybe.map (\card -> { card | show = True })
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

                            Nothing ->
                                ( model, Cmd.none )

                    DoubleCardInTableHandToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players bGameInProgressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
                                        let
                                            currentPlayer : Maybe BPlayer
                                            currentPlayer =
                                                List.Extra.find ((==) sessionId << .sessionId) players

                                            cardFromPlayer : Maybe Card
                                            cardFromPlayer =
                                                currentPlayer
                                                    |> Maybe.andThen (\p -> List.Extra.getAt cardIndex p.tableHand)

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

                            Nothing ->
                                ( model, Cmd.none )

                    LookAtCardInTableHandToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
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
                                        ( { model | errors = ("2: " ++ DebugApp.bGameInProgressLogs "LookAtCardInTableHandToBackend" bGameStatus) :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                            Nothing ->
                                ( { model | errors = "3: LookAtCardInTableHandToBackend" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                    ChooseOwnCardToSwitchToBackend cardIndex ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards ChooseOwnCardToSwitch)) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                        if sessionId == bPlayer.sessionId then
                                            let
                                                newGameStatus : BGameStatus
                                                newGameStatus =
                                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OwnCardChosen cardIndex))) lastMoveIsDouble canUsePowerFromLastPlayer
                                            in
                                            updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players (Just AnimationSwitchCard)

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    ChooseOpponentCardToSwitchToBackend ( opponentSessionId, opponentCardIndex ) ->
                        case maybeGame of
                            Just game ->
                                case game.status of
                                    BGameInProgress maybeTamalouOwner drawPile discardPile players (BPlayerToPlay bPlayer (BPlayerSwitch2Cards (OwnCardChosen cardIndex))) lastMoveIsDouble canUsePowerFromLastPlayer ->
                                        let
                                            -- maybeNextPlayer : Maybe BPlayer
                                            -- maybeNextPlayer =
                                            --     nextPlayer maybeTamalouOwner bPlayer.sessionId newPlayers
                                            newGameStatus : BGameStatus
                                            newGameStatus =
                                                -- case maybeNextPlayer of
                                                --     Just nextPlayer_ ->
                                                BGameInProgress maybeTamalouOwner
                                                    drawPile
                                                    discardPile
                                                    newPlayers
                                                    (BPlayerToPlay bPlayer <|
                                                        BPlayerSwitch2Cards (OpponentCardChosen cardIndex { sessionId = opponentSessionId, index = opponentCardIndex } Three)
                                                    )
                                                    -- (BPlayerToPlay nextPlayer_
                                                    --     (BWaitingPlayerAction
                                                    --         (if canUsePowerFromLastPlayer then
                                                    --             Just Card.Switch2Cards
                                                    --          else
                                                    --             Nothing
                                                    --         )
                                                    --     )
                                                    -- )
                                                    lastMoveIsDouble
                                                    canUsePowerFromLastPlayer

                                            -- Nothing ->
                                            --     BGameInProgress maybeTamalouOwner drawPile discardPile newPlayers (BEndTimerRunning Five) lastMoveIsDouble False
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
                                        updateGameStateAndNotifyPlayers model game.urlPath ( newGameStatus, game.seed ) players (Just AnimationSwitchCard)

                                    _ ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                    PowerIsUsedToBackend ->
                        case maybeGame of
                            Just game ->
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
                                                            -- This should not be possible to reach this case if
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
                                        ( { model | errors = ("2: " ++ DebugApp.bGameInProgressLogs "PowerIsUsedToBackend: " bGameStatus) :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                            Nothing ->
                                ( { model | errors = "3: PowerIsUsedToBackend: Game not found" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                    PowerIsNotUsedToBackend ->
                        case maybeGame of
                            Just game ->
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
                                        ( { model | errors = ("2: " ++ DebugApp.bGameInProgressLogs "PowerIsNotUsedToBackend: " bGameStatus) :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                            Nothing ->
                                ( { model | errors = "3: PowerIsNotUsedToBackend: Game not found" :: errors }, Lamdera.broadcast (UpdateAdminToFrontend errors) )

                    TamalouToBackend ->
                        case maybeGame of
                            Just game ->
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

                            Nothing ->
                                ( model, Cmd.none )

                    SendMessageToBackend newMessage ->
                        case maybeGame of
                            Just game ->
                                let
                                    newChat : List ( String, String )
                                    newChat =
                                        game.chat ++ [ ( playerName_, newMessage ) ]

                                    newGame : BGame
                                    newGame =
                                        { game | chat = newChat }

                                    playerName_ : String
                                    playerName_ =
                                        playerName sessionId game
                                in
                                ( { model | games = updateGame newGame games }
                                , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateChatToFrontend newChat) (bPlayersFromFGame newGame)
                                )

                            Nothing ->
                                ( model, Cmd.none )

        ConnectToAdminToBackend ->
            ( model, Lamdera.sendToFrontend clientId (UpdateAdminToFrontend errors) )


updateGame : BGame -> List BGame -> List BGame
updateGame newGame games =
    List.Extra.updateIf
        ((==) newGame.urlPath << .urlPath)
        (always newGame)
        games


updateGameStateAndNotifyPlayers : BackendModel -> String -> ( BGameStatus, Random.Seed ) -> List BPlayer -> Maybe PlayerAction -> ( BackendModel, Cmd BackendMsg )
updateGameStateAndNotifyPlayers ({ games } as model) urlPath ( newGameStatus, newSeed ) players maybePlayerAction =
    ( { model | games = updateGameStatus urlPath ( newGameStatus, newSeed ) games }
    , Cmd.batch <| List.map (\player -> Lamdera.sendToFrontend player.clientId <| UpdateGameStatusToFrontend (toFGame (Just player.sessionId) newGameStatus) maybePlayerAction) players
    )


updateGameStatus : String -> ( BGameStatus, Random.Seed ) -> List BGame -> List BGame
updateGameStatus urlPath ( newGameStatus, newSeed ) games =
    List.Extra.updateIf
        ((==) urlPath << .urlPath)
        (\g -> { g | status = newGameStatus, seed = newSeed })
        games


assignRanks : Maybe SessionId -> List BPlayer -> List ( BPlayer, Int )
assignRanks maybeTamalouOwner players =
    players
        |> List.sortWith
            (\playerA playerB ->
                case compare (Card.tableHandScore playerA.tableHand) (Card.tableHandScore playerB.tableHand) of
                    EQ ->
                        case compare (List.length playerA.tableHand) (List.length playerB.tableHand) of
                            EQ ->
                                case maybeTamalouOwner of
                                    Just ownerId ->
                                        if ownerId == playerA.sessionId then
                                            LT

                                        else if ownerId == playerB.sessionId then
                                            GT

                                        else
                                            EQ

                                    Nothing ->
                                        EQ

                            ord ->
                                ord

                    ord ->
                        ord
            )
        |> List.foldl
            (\player ( ( acc, lastScore ), ( lastCount, nextRank ) ) ->
                let
                    currentCount : Int
                    currentCount =
                        List.length player.tableHand

                    currentScore : Int
                    currentScore =
                        Card.tableHandScore player.tableHand

                    isOwner : Bool
                    isOwner =
                        Just player.sessionId == maybeTamalouOwner

                    ( rank, newNextRank ) =
                        if lastScore == currentScore && lastCount == currentCount && not isOwner then
                            ( nextRank - 1, nextRank )

                        else
                            ( nextRank, nextRank + 1 )
                in
                ( ( ( player, rank ) :: acc, currentScore ), ( currentCount, newNextRank ) )
            )
            ( ( [], -1 ), ( -1, 1 ) )
        |> Tuple.first
        |> Tuple.first
        |> List.reverse
