module GameLogics exposing (..)

import Card
import Game exposing (BDrawPile)
import Lamdera exposing (SessionId)
import List.Extra as List
import Player exposing (BPlayer, CurrentPlayer, toCurrentPlayer)


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


nextPlayer : Maybe SessionId -> SessionId -> List BPlayer -> Maybe CurrentPlayer
nextPlayer maybeTamalouOwnerSessionId sessionId players =
    List.findIndex ((==) sessionId << .sessionId) players
        |> Maybe.andThen (\index_ -> List.getAt (modBy (List.length players) (index_ + 1)) players)
        |> Maybe.andThen
            (\p ->
                if maybeTamalouOwnerSessionId == Just p.sessionId then
                    Nothing

                else
                    Just p
            )
        |> Maybe.map toCurrentPlayer


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
