module Game exposing (..)

import Card exposing (Card, FCard(..), showAllCards, toFCard)
import Counter exposing (Counter)
import Lamdera exposing (SessionId)
import List.Extra
import Player exposing (BPlayer, BPlayerToPlayStatus(..), FPlayer, FPlayerToPlayStatus(..), showTamalouOwnerCards, stopDisplayCards, toFPlayer)
import Random
import Utils.List exposing (findAndRearrange)


type alias BDrawPile =
    List Card


type alias DiscardPile =
    List Card


type alias FDrawPile =
    List FCard


type alias FTableHand =
    List FCard


type alias BGame =
    { urlPath : String
    , status : BGameStatus
    , chat : List ( String, String )
    , seed : Random.Seed
    }


type BGameInProgressStatus
    = BStartTimerRunning Counter
    | BPlayerToPlay BPlayer BPlayerToPlayStatus
    | BEndTimerRunning Counter


type BGameStatus
    = BWaitingForPlayers (List BPlayer)
    | BGameInProgress (Maybe SessionId) BDrawPile DiscardPile (List BPlayer) BGameInProgressStatus Bool Bool
    | BGameEnded (List ( BPlayer, Int ))


type FGame
    = FWaitingForPlayers (List FPlayer)
    | FGameInProgress (Maybe TamalouOwner) FTableHand FDrawPile DiscardPile (List FPlayer) FGameInProgressStatus
    | FGameEnded (List ( FPlayer, Int ))
    | FGameAlreadyStartedWithoutYou


type FGameInProgressStatus
    = FStartTimerRunning Counter
    | FPlayerToPlay FPlayer FPlayerToPlayStatus
    | FYourTurn FPlayerToPlayStatus
    | FEndTimerRunning Counter


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


type alias TamalouOwner =
    { sessionId : SessionId
    , tableHand : List Card
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
