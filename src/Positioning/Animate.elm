module Positioning.Animate exposing (..)

import Animator.Timeline as Timeline exposing (Timeline)
import Card exposing (Card, FCard(..))
import Lamdera exposing (SessionId)
import List.Extra
import Positioning.Helpers exposing (findCardPosition, fixReverseSpinningEffectRotation, fixSpinningEffectRotation)
import Positioning.Positioning exposing (updateCardPosition, updateOpponentsDisposition)
import Positioning.Types exposing (GBPosition)
import Simple.Animation exposing (steps)
import Time exposing (Posix)
import Types exposing (AddOrRemove(..), FrontendModel, GameDisposition(..), OpponentsDisposition, PlayerActionAnimation(..), PositionedPlayer, Positions)
import Ui.Anim as Anim


applyDoubleAnimationToOwnCard : Int -> ( Card, Timeline GBPosition ) -> Positions -> List ( FCard, Timeline GBPosition ) -> List ( FCard, Timeline GBPosition )
applyDoubleAnimationToOwnCard cardIndex ( cardToAnimate, oldCardPosition ) { discardPilePosition, ownCardsDisposition } oldOwnCardsDisposition =
    let
        addCardOrRemoveCard : AddOrRemove
        addCardOrRemoveCard =
            if List.length ownCardsDisposition > List.length oldOwnCardsDisposition then
                Add

            else
                Remove

        applyTransitionToCardInHand : Int -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
        applyTransitionToCardInHand index ( card, position ) =
            if index == cardIndex then
                if addCardOrRemoveCard == Add then
                    ( FaceUp cardToAnimate
                    , let
                        newCardPosition : Timeline GBPosition
                        newCardPosition =
                            case List.Extra.getAt index ownCardsDisposition of
                                Just ( _, newPos ) ->
                                    newPos

                                Nothing ->
                                    position

                        steps : List (Timeline.Step GBPosition)
                        steps =
                            [ Timeline.transitionTo (Anim.ms <| animDuration) discardPilePosition
                            , Timeline.transitionTo (Anim.ms <| animDuration) (Timeline.current newCardPosition)
                            ]
                      in
                      Timeline.scale 0.5 <| Timeline.queue steps oldCardPosition
                    )

                else
                    ( FaceUp cardToAnimate, Timeline.to (Anim.ms animDuration) discardPilePosition oldCardPosition )

            else
                let
                    newPosition : Timeline GBPosition
                    newPosition =
                        if addCardOrRemoveCard == Remove && index > cardIndex then
                            case List.Extra.getAt (index - 1) ownCardsDisposition of
                                Just ( _, newPos ) ->
                                    newPos

                                Nothing ->
                                    position

                        else
                            case List.Extra.getAt index ownCardsDisposition of
                                Just ( _, newPos ) ->
                                    newPos

                                Nothing ->
                                    position
                in
                ( card, Timeline.to (Anim.ms animDuration) (Timeline.current newPosition) position )
    in
    List.indexedMap applyTransitionToCardInHand oldOwnCardsDisposition


applyDoubleAnimationsToOpponent : SessionId -> Int -> ( Card, Timeline GBPosition ) -> Positions -> OpponentsDisposition -> OpponentsDisposition
applyDoubleAnimationsToOpponent sessionId cardIndex ( cardToAnimate, oldCardPosition ) { discardPilePosition, opponentsDisposition } { leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer } =
    let
        updateHand : PositionedPlayer -> PositionedPlayer -> PositionedPlayer
        updateHand newPositionedPlayer positionedPlayer =
            if sessionId == positionedPlayer.player.sessionId then
                let
                    addCardOrRemoveCard : AddOrRemove
                    addCardOrRemoveCard =
                        if List.length newPositionedPlayer.positionedTableHand > List.length positionedPlayer.positionedTableHand then
                            Add

                        else
                            Remove

                    applyTransitionToCardInHand : Int -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
                    applyTransitionToCardInHand index ( card, position ) =
                        if index == cardIndex then
                            if addCardOrRemoveCard == Add then
                                ( FaceUp cardToAnimate
                                , let
                                    newCardPosition : Timeline GBPosition
                                    newCardPosition =
                                        case List.Extra.getAt index newPositionedPlayer.positionedTableHand of
                                            Just ( _, newPos ) ->
                                                newPos

                                            Nothing ->
                                                position

                                    steps : List (Timeline.Step GBPosition)
                                    steps =
                                        [ Timeline.transitionTo (Anim.ms <| animDuration) discardPilePosition
                                        , Timeline.transitionTo (Anim.ms <| animDuration) (Timeline.current newCardPosition)
                                        ]
                                  in
                                  Timeline.scale 0.5 <| Timeline.queue steps oldCardPosition
                                )

                            else
                                ( FaceUp cardToAnimate, Timeline.to (Anim.ms animDuration) discardPilePosition oldCardPosition )

                        else
                            let
                                newPosition : Timeline GBPosition
                                newPosition =
                                    if addCardOrRemoveCard == Remove && index > cardIndex then
                                        case List.Extra.getAt (index - 1) newPositionedPlayer.positionedTableHand of
                                            Just ( _, newPos ) ->
                                                newPos

                                            Nothing ->
                                                position

                                    else
                                        case List.Extra.getAt index newPositionedPlayer.positionedTableHand of
                                            Just ( _, newPos ) ->
                                                newPos

                                            Nothing ->
                                                position
                            in
                            ( card, Timeline.to (Anim.ms animDuration) (Timeline.current newPosition) position )
                in
                { positionedPlayer | positionedTableHand = List.indexedMap applyTransitionToCardInHand positionedPlayer.positionedTableHand }

            else
                positionedPlayer
    in
    { leftPlayer = Maybe.map2 updateHand opponentsDisposition.leftPlayer leftPlayer
    , topLeftPlayer = Maybe.map2 updateHand opponentsDisposition.topLeftPlayer topLeftPlayer
    , topRightPlayer = Maybe.map2 updateHand opponentsDisposition.topRightPlayer topRightPlayer
    , rightPlayer = Maybe.map2 updateHand opponentsDisposition.rightPlayer rightPlayer
    }


applyReplaceCardAnimationsToOpponent : SessionId -> Int -> ( Card, Timeline GBPosition ) -> Positions -> OpponentsDisposition -> OpponentsDisposition
applyReplaceCardAnimationsToOpponent sessionId cardIndex ( cardToAnimate, oldCardPosition ) { discardPilePosition } { leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer } =
    let
        updateHand : PositionedPlayer -> PositionedPlayer
        updateHand positionedPlayer =
            if sessionId == positionedPlayer.player.sessionId then
                let
                    applyTransitionToCardInHand : Int -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
                    applyTransitionToCardInHand index ( card, position ) =
                        if index == cardIndex then
                            ( FaceUp cardToAnimate, Timeline.to (Anim.ms animDuration) discardPilePosition oldCardPosition )

                        else
                            ( card, position )
                in
                { positionedPlayer | positionedTableHand = List.indexedMap applyTransitionToCardInHand positionedPlayer.positionedTableHand }

            else
                positionedPlayer
    in
    { leftPlayer = Maybe.map updateHand leftPlayer
    , topLeftPlayer = Maybe.map updateHand topLeftPlayer
    , topRightPlayer = Maybe.map updateHand topRightPlayer
    , rightPlayer = Maybe.map updateHand rightPlayer
    }


applyReplaceCardAnimationsToOwnCard : Int -> ( Card, Timeline GBPosition ) -> Positions -> List ( FCard, Timeline GBPosition ) -> List ( FCard, Timeline GBPosition )
applyReplaceCardAnimationsToOwnCard cardIndex ( cardToAnimate, oldCardPosition ) { discardPilePosition } oldOwnCardsDisposition =
    let
        applyTransitionToCardInHand : Int -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
        applyTransitionToCardInHand index ( card, position ) =
            if index == cardIndex then
                ( FaceUp cardToAnimate, Timeline.to (Anim.ms animDuration) discardPilePosition oldCardPosition )

            else
                ( card, position )
    in
    List.indexedMap applyTransitionToCardInHand oldOwnCardsDisposition


applySwitchAnimationToOwnCard : Int -> Timeline GBPosition -> List ( FCard, Timeline GBPosition ) -> List ( FCard, Timeline GBPosition )
applySwitchAnimationToOwnCard cardIndex oldCardPosition oldOwnCardsDisposition =
    let
        applyTransitionToCardInHand : Int -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
        applyTransitionToCardInHand index ( card, position ) =
            if index == cardIndex then
                ( card, Timeline.to (Anim.ms animDuration) (Timeline.current oldCardPosition) position )

            else
                ( card, position )
    in
    List.indexedMap applyTransitionToCardInHand oldOwnCardsDisposition


applySwitchAnimationToOpponent : SessionId -> Int -> Timeline GBPosition -> OpponentsDisposition -> OpponentsDisposition
applySwitchAnimationToOpponent sessionId cardIndex oldCardPosition { leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer } =
    let
        updateHand : PositionedPlayer -> PositionedPlayer
        updateHand positionedPlayer =
            if sessionId == positionedPlayer.player.sessionId then
                let
                    applyTransitionToCardInHand : Int -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
                    applyTransitionToCardInHand index ( card, position ) =
                        if index == cardIndex then
                            ( card, Timeline.to (Anim.ms animDuration) (Timeline.current oldCardPosition) position )

                        else
                            ( card, position )
                in
                { positionedPlayer | positionedTableHand = List.indexedMap applyTransitionToCardInHand positionedPlayer.positionedTableHand }

            else
                positionedPlayer
    in
    { leftPlayer = Maybe.map updateHand leftPlayer
    , topLeftPlayer = Maybe.map updateHand topLeftPlayer
    , topRightPlayer = Maybe.map updateHand topRightPlayer
    , rightPlayer = Maybe.map updateHand rightPlayer
    }


applyPenaltyAnimationToOpponent : SessionId -> Positions -> Timeline GBPosition
applyPenaltyAnimationToOpponent sessionId { drawPilePosition, opponentsDisposition } =
    [ opponentsDisposition.leftPlayer, opponentsDisposition.topLeftPlayer, opponentsDisposition.topRightPlayer, opponentsDisposition.rightPlayer ]
        |> List.filterMap identity
        |> List.Extra.find (\player -> player.player.sessionId == sessionId)
        |> Maybe.andThen (.positionedTableHand >> List.Extra.last >> Maybe.map Tuple.second)
        |> Maybe.map (\newCardPosition -> Timeline.to (Anim.ms animDuration) (Timeline.current newCardPosition) (Timeline.init <| fixSpinningEffectRotation drawPilePosition))
        |> Maybe.withDefault (Timeline.init drawPilePosition)


applyPenaltyAnimationToUs : Positions -> Timeline GBPosition
applyPenaltyAnimationToUs { drawPilePosition, ownCardsDisposition } =
    ownCardsDisposition
        |> List.Extra.last
        |> Maybe.map Tuple.second
        |> Maybe.map (\newCardPosition -> Timeline.to (Anim.ms animDuration) (Timeline.current newCardPosition) (Timeline.init <| fixSpinningEffectRotation drawPilePosition))
        |> Maybe.withDefault (Timeline.init drawPilePosition)


updateEveryTimelineOnFrame : FrontendModel -> Posix -> FrontendModel
updateEveryTimelineOnFrame model posix =
    case model.gameDisposition of
        NotCalculated ->
            model

        Calculated positions ->
            let
                updatedPositions : Positions
                updatedPositions =
                    { positions
                        | cardsFromDrawPileMovingPositions =
                            positions.cardsFromDrawPileMovingPositions
                                |> List.map (Timeline.update posix)
                                |> List.filter Timeline.isRunning
                        , drewCardMovingPosition =
                            if Timeline.isRunning positions.drewCardMovingPosition then
                                Timeline.update posix positions.drewCardMovingPosition

                            else
                                positions.drewCardMovingPosition
                        , cardFromDiscardPileMovingPositions =
                            positions.cardFromDiscardPileMovingPositions
                                |> Maybe.map (Timeline.update posix)
                                |> Maybe.andThen
                                    (\a ->
                                        if Timeline.isRunning a then
                                            Just a

                                        else
                                            Nothing
                                    )
                        , opponentsDisposition =
                            positions.opponentsDisposition
                                |> updateOpponentsDisposition posix
                        , ownCardsDisposition =
                            positions.ownCardsDisposition
                                |> List.map (updateCardPosition posix)
                    }
            in
            { model
                | gameDisposition = Calculated updatedPositions
            }


animatePlayerAction : PlayerActionAnimation -> Positions -> FrontendModel -> FrontendModel
animatePlayerAction playerAction newGameDisposition fModel =
    case fModel.gameDisposition of
        Calculated positions ->
            case playerAction of
                AnimationDrawCardFromDeck ->
                    { fModel
                        | gameDisposition =
                            Calculated
                                { positions
                                    | cardsFromDrawPileMovingPositions =
                                        (Timeline.init positions.drawPilePosition
                                            |> Timeline.to (Anim.ms animDuration) (Timeline.current positions.drewCardMovingPosition)
                                        )
                                            :: positions.cardsFromDrawPileMovingPositions
                                }
                    }

                AnimationDrawCardFromDiscardPile ->
                    { fModel
                        | gameDisposition =
                            Calculated
                                { positions
                                    | cardFromDiscardPileMovingPositions =
                                        Just
                                            (Timeline.init positions.discardPilePosition
                                                |> Timeline.to (Anim.ms animDuration) (Timeline.current positions.drewCardMovingPosition)
                                            )
                                }
                    }

                AnimationReplaceCardInTableHand sessionId cardIndex discardedCard ->
                    let
                        maybeOpponentOldCardPosition : Maybe (Timeline GBPosition)
                        maybeOpponentOldCardPosition =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId cardIndex
                    in
                    case maybeOpponentOldCardPosition of
                        Just oldCardPosition ->
                            { fModel
                                | gameDisposition =
                                    Calculated
                                        { positions
                                            | drewCardMovingPosition =
                                                Timeline.to (Anim.ms animDuration) (fixReverseSpinningEffectRotation <| Timeline.current oldCardPosition) positions.drewCardMovingPosition
                                            , opponentsDisposition =
                                                positions.opponentsDisposition
                                                    |> applyReplaceCardAnimationsToOpponent sessionId cardIndex ( discardedCard, oldCardPosition ) newGameDisposition
                                        }
                            }

                        Nothing ->
                            let
                                maybeOwnOldCardPosition : Maybe (Timeline GBPosition)
                                maybeOwnOldCardPosition =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex
                                        |> Maybe.map Tuple.second
                            in
                            case ( fModel.sessionId == Just sessionId, maybeOwnOldCardPosition ) of
                                ( True, Just oldCardPosition ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | drewCardMovingPosition =
                                                        Timeline.to (Anim.ms animDuration) (fixReverseSpinningEffectRotation <| Timeline.current oldCardPosition) positions.drewCardMovingPosition
                                                    , ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applyReplaceCardAnimationsToOwnCard cardIndex ( discardedCard, oldCardPosition ) newGameDisposition
                                                }
                                    }

                                _ ->
                                    fModel

                AnimationDoubleCardSuccess sessionId cardIndex card ->
                    let
                        maybeOpponentOldCardPosition : Maybe (Timeline GBPosition)
                        maybeOpponentOldCardPosition =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId cardIndex
                    in
                    case maybeOpponentOldCardPosition of
                        Just oldCardPosition ->
                            { fModel
                                | gameDisposition =
                                    Calculated
                                        { positions
                                            | opponentsDisposition =
                                                positions.opponentsDisposition
                                                    |> applyDoubleAnimationsToOpponent sessionId cardIndex ( card, oldCardPosition ) newGameDisposition
                                        }
                            }

                        Nothing ->
                            let
                                maybeOwnOldCardPosition : Maybe (Timeline GBPosition)
                                maybeOwnOldCardPosition =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex
                                        |> Maybe.map Tuple.second
                                        |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
                            in
                            case ( fModel.sessionId == Just sessionId, maybeOwnOldCardPosition ) of
                                ( True, Just oldCardPosition ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applyDoubleAnimationToOwnCard cardIndex ( card, oldCardPosition ) newGameDisposition
                                                }
                                    }

                                _ ->
                                    fModel

                AnimationDoubleCardFailed sessionId cardIndex card ->
                    let
                        maybeOpponentOldCardPosition : Maybe (Timeline GBPosition)
                        maybeOpponentOldCardPosition =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId cardIndex

                        -- |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
                    in
                    case maybeOpponentOldCardPosition of
                        Just oldCardPosition ->
                            { fModel
                                | gameDisposition =
                                    Calculated
                                        { positions
                                            | cardsFromDrawPileMovingPositions =
                                                applyPenaltyAnimationToOpponent sessionId newGameDisposition :: positions.cardsFromDrawPileMovingPositions
                                            , opponentsDisposition =
                                                positions.opponentsDisposition
                                                    |> applyDoubleAnimationsToOpponent sessionId cardIndex ( card, oldCardPosition ) newGameDisposition
                                        }
                            }

                        Nothing ->
                            let
                                maybeOwnOldCardPosition : Maybe (Timeline GBPosition)
                                maybeOwnOldCardPosition =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex
                                        |> Maybe.map Tuple.second
                            in
                            case ( fModel.sessionId == Just sessionId, maybeOwnOldCardPosition ) of
                                ( True, Just oldCardPosition ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | cardsFromDrawPileMovingPositions =
                                                        applyPenaltyAnimationToUs newGameDisposition :: positions.cardsFromDrawPileMovingPositions
                                                    , ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applyDoubleAnimationToOwnCard cardIndex ( card, oldCardPosition ) newGameDisposition
                                                }
                                    }

                                _ ->
                                    fModel

                AnimationSwitchCards ( sessionId1, cardIndex1 ) ( sessionId2, cardIndex2 ) ->
                    let
                        maybeOpponentOldCardPosition1 : Maybe (Timeline GBPosition)
                        maybeOpponentOldCardPosition1 =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId1 cardIndex1

                        maybeOpponentOldCardPosition2 : Maybe (Timeline GBPosition)
                        maybeOpponentOldCardPosition2 =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId2 cardIndex2
                    in
                    case ( maybeOpponentOldCardPosition1, maybeOpponentOldCardPosition2 ) of
                        ( Just oldCardPosition1, Just oldCardPosition2 ) ->
                            { fModel
                                | gameDisposition =
                                    Calculated
                                        { positions
                                            | opponentsDisposition =
                                                positions.opponentsDisposition
                                                    |> applySwitchAnimationToOpponent sessionId1 cardIndex1 oldCardPosition2
                                                    |> applySwitchAnimationToOpponent sessionId2 cardIndex2 oldCardPosition1
                                        }
                            }

                        ( Just oldCardPosition1, Nothing ) ->
                            let
                                maybeOwnOldCardPosition2 : Maybe (Timeline GBPosition)
                                maybeOwnOldCardPosition2 =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex2
                                        |> Maybe.map Tuple.second
                            in
                            case ( fModel.sessionId == Just sessionId2, maybeOwnOldCardPosition2 ) of
                                ( True, Just oldCardPosition2 ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | opponentsDisposition =
                                                        positions.opponentsDisposition
                                                            |> applySwitchAnimationToOpponent sessionId1 cardIndex1 oldCardPosition2
                                                    , ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applySwitchAnimationToOwnCard cardIndex2 oldCardPosition1
                                                }
                                    }

                                _ ->
                                    fModel

                        ( Nothing, Just oldCardPosition2 ) ->
                            let
                                maybeOwnOldCardPosition1 : Maybe (Timeline GBPosition)
                                maybeOwnOldCardPosition1 =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex1
                                        |> Maybe.map Tuple.second
                            in
                            case ( fModel.sessionId == Just sessionId1, maybeOwnOldCardPosition1 ) of
                                ( True, Just oldCardPosition1 ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | opponentsDisposition =
                                                        positions.opponentsDisposition
                                                            |> applySwitchAnimationToOpponent sessionId2 cardIndex2 oldCardPosition1
                                                    , ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applySwitchAnimationToOwnCard cardIndex1 oldCardPosition2
                                                }
                                    }

                                _ ->
                                    fModel

                        ( Nothing, Nothing ) ->
                            fModel

                -- FOR THIS CASE: there are 2 possibilities: 2 opponents cards switching or our own card switched with an opponent card.
                AnimationDiscardCard ->
                    { fModel
                        | gameDisposition =
                            Calculated
                                { positions
                                    | drewCardMovingPosition =
                                        positions.drewCardMovingPosition
                                            |> Timeline.to (Anim.ms animDuration) positions.discardPilePosition
                                }
                    }

                NoPlayerAction ->
                    fModel

        -- SAME AS DOUBLE FAILED
        -- AnimationTamalouFailed sessionId ->
        --     let
        --         maybeOpponentOldCardPosition : Maybe (Timeline GBPosition)
        --         maybeOpponentOldCardPosition =
        --             positions.opponentsDisposition
        --                 |> findCardPosition sessionId 0
        --                 |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
        --     in
        --     case maybeOpponentOldCardPosition of
        --         Just oldCardPosition ->
        --             { fModel
        --                 | gameDisposition =
        --                     Calculated
        --                         { positions
        --                             | cardsFromDrawPileMovingPositions =
        --                                 applyPenaltyAnimationToOpponent sessionId newGameDisposition :: positions.cardsFromDrawPileMovingPositions
        --                             , opponentsDisposition =
        --                                 positions.opponentsDisposition
        --                                     |> applyDoubleAnimationsToOpponent sessionId 0 ( FaceDown, oldCardPosition ) newGameDisposition
        --                         }
        --             }
        --         Nothing ->
        --             let
        --                 maybeOwnOldCardPosition : Maybe (Timeline GBPosition)
        --                 maybeOwnOldCardPosition =
        --                     positions.ownCardsDisposition
        --                         |> List.Extra.getAt 0
        --                         |> Maybe.map Tuple.second
        --             in
        --             case ( fModel.sessionId == Just sessionId, maybeOwnOldCardPosition ) of
        --                 ( True, Just oldCardPosition ) ->
        --                     { fModel
        --                         | gameDisposition =
        --                             Calculated
        --                                 { positions
        --                                     | cardsFromDrawPileMovingPositions =
        --                                         applyPenaltyAnimationToUs newGameDisposition :: positions.cardsFromDrawPileMovingPositions
        --                                     , ownCardsDisposition =
        --                                         positions.ownCardsDisposition
        --                                             |> applyDoubleAnimationToOwnCard 0 ( FaceDown, oldCardPosition ) newGameDisposition
        --                                 }
        --                     }
        --                 _ ->
        --                     fModel
        _ ->
            fModel


animDuration : Float
animDuration =
    500
