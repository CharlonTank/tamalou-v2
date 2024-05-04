module Positioning.Positioning exposing (calculateGameDisposition, getGBPosition, moveUpBasedOnRotation, updateCardPosition, updateOpponentsDisposition)

import Animator.Timeline as Timeline exposing (Timeline)
import Animator.Transition
import Animator.Value
import Card exposing (FCard)
import Internal.Style2 exposing (toRadians)
import Player exposing (FPlayer)
import Positioning.Helpers exposing (wantedSpinningRotationValue)
import Positioning.Types exposing (GBPosition, OpponentDisposition(..), VisibleAngle(..))
import Time exposing (Posix)
import Types exposing (OpponentsDisposition, PositionedPlayer, Positions)
import Ui


calculateGameDisposition : { height : Int, width : Int } -> List FPlayer -> List FCard -> Positions
calculateGameDisposition viewPort opponents ownCards =
    { drawPilePosition = calculateCardPosition viewPort.width viewPort.height 0.35 0.35
    , cardsFromDrawPileMovingPositions = []
    , drewCardMovingPosition = Timeline.init (calculateCardPosition viewPort.width viewPort.height 0.5 0.35)
    , middleTextPosition = calculateCardPosition viewPort.width viewPort.height 0.5 0.74
    , discardPilePosition = calculateCardPosition viewPort.width viewPort.height 0.65 0.35
    , cardFromDiscardPileMovingPositions = Nothing
    , playAgainOrPassPosition = calculatePlayAgainOrPassPosition viewPort.width viewPort.height
    , opponentsDisposition = toOpponentsDisposition viewPort.width opponents
    , ownCardsDisposition = toOwnCardsDisposition viewPort ownCards
    }


getGBPosition : Timeline GBPosition -> GBPosition
getGBPosition timeline =
    { x = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .x)
    , y = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .y)
    , width_ = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .width_)
    , height_ = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .height_)
    , rotation = Ui.radians <| Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << toRadians << .rotation)
    }


moveUpBasedOnRotation : Timeline GBPosition -> Timeline GBPosition
moveUpBasedOnRotation timeline =
    let
        position : GBPosition
        position =
            getGBPosition timeline
    in
    (case toVisibleAngle position.rotation of
        AngleZero ->
            { position | y = position.y + 20 }

        AnglePiOverTwo ->
            { position | x = position.x + 20 }

        AnglePi ->
            { position | y = position.y - 20 }

        AngleThreePiOverTwo ->
            { position | x = position.x - 20 }
    )
        |> Timeline.init


updateCardPosition : Posix -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
updateCardPosition posix ( card, cardTimeline ) =
    ( card, Timeline.update posix cardTimeline )


updateOpponentsDisposition : Posix -> OpponentsDisposition -> OpponentsDisposition
updateOpponentsDisposition posix opponentsDisposition =
    { opponentsDisposition
        | leftPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.leftPlayer
        , topLeftPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.topLeftPlayer
        , topRightPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.topRightPlayer
        , rightPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.rightPlayer
    }


updatePositionedPlayer : Posix -> PositionedPlayer -> PositionedPlayer
updatePositionedPlayer posix positionedPlayer =
    { positionedPlayer
        | positionedTableHand =
            positionedPlayer.positionedTableHand
                |> List.map (updateCardPosition posix)
    }


toVisibleAngle : Ui.Angle -> VisibleAngle
toVisibleAngle angle =
    case truncate <| (toRadians angle - wantedSpinningRotationValue) of
        0 ->
            AngleZero

        1 ->
            AnglePiOverTwo

        3 ->
            AnglePi

        4 ->
            AngleThreePiOverTwo

        _ ->
            AngleZero


toOwnCardsDisposition : { height : Int, width : Int } -> List FCard -> List ( FCard, Timeline GBPosition )
toOwnCardsDisposition viewPort ownCards =
    let
        cardHeight : Float
        cardHeight =
            cardWidth * heightCardRatio

        cardPanel : Float
        cardPanel =
            (if totalCards == 1 then
                0.11

             else if totalCards == 2 then
                0.26

             else if totalCards == 3 then
                0.38

             else if totalCards == 4 then
                0.5

             else if totalCards == 5 then
                0.6

             else
                0.7
            )
                * toFloat viewPort.width

        cardWidth : Float
        cardWidth =
            (cardPanel - totalSpaceBetweenCards) / toFloat totalCards

        spaceBetweenEachCard : Float
        spaceBetweenEachCard =
            64 / (toFloat <| List.length ownCards)

        startX : Float
        startX =
            (toFloat viewPort.width - (toFloat totalCards * cardWidth + totalSpaceBetweenCards)) / 2

        totalCards : Int
        totalCards =
            List.length ownCards

        totalSpaceBetweenCards : Float
        totalSpaceBetweenCards =
            toFloat (totalCards - 1) * spaceBetweenEachCard
    in
    List.indexedMap
        (\i c ->
            ( c
            , Timeline.init
                { x = startX + toFloat i * (cardWidth + spaceBetweenEachCard)
                , y = toFloat viewPort.height - cardHeight - 20
                , width_ = cardWidth
                , height_ = cardHeight
                , rotation = Ui.radians wantedSpinningRotationValue
                }
            )
        )
        ownCards


toOpponentsDisposition : Int -> List FPlayer -> OpponentsDisposition
toOpponentsDisposition screenWidth players =
    case players of
        [] ->
            { leftPlayer = Nothing, topLeftPlayer = Nothing, topRightPlayer = Nothing, rightPlayer = Nothing }

        [ oneOpponent ] ->
            { leftPlayer = Nothing, topLeftPlayer = Just <| positionOpponent screenWidth oneOpponent TopLeftPlayer, topRightPlayer = Nothing, rightPlayer = Nothing }

        [ firstOpponent, secondOpponent ] ->
            { leftPlayer = Nothing, topLeftPlayer = Just <| positionOpponent screenWidth firstOpponent TopLeftPlayer, topRightPlayer = Just <| positionOpponent screenWidth secondOpponent TopRightPlayer, rightPlayer = Nothing }

        [ firstOpponent, secondOpponent, thirdOpponent ] ->
            { leftPlayer = Just <| positionOpponent screenWidth firstOpponent LeftPlayer, topLeftPlayer = Just <| positionOpponent screenWidth secondOpponent TopLeftPlayer, topRightPlayer = Just <| positionOpponent screenWidth thirdOpponent TopRightPlayer, rightPlayer = Nothing }

        [ firstOpponent, secondOpponent, thirdOpponent, fourthOpponent ] ->
            { leftPlayer = Just <| positionOpponent screenWidth firstOpponent LeftPlayer, topLeftPlayer = Just <| positionOpponent screenWidth secondOpponent TopLeftPlayer, topRightPlayer = Just <| positionOpponent screenWidth thirdOpponent TopRightPlayer, rightPlayer = Just <| positionOpponent screenWidth fourthOpponent RightPlayer }

        _ ->
            { leftPlayer = Nothing, topLeftPlayer = Nothing, topRightPlayer = Nothing, rightPlayer = Nothing }


heightCardRatio : Float
heightCardRatio =
    380 / 250


cardWidthInMiddle : Int -> Float
cardWidthInMiddle widthOfScreen =
    toFloat widthOfScreen / 10


calculateCardPosition : Int -> Int -> Float -> Float -> GBPosition
calculateCardPosition screenWidth screenHeight xOffset yOffset =
    let
        ( width_, height_ ) =
            ( cardWidthInMiddle screenWidth, cardWidthInMiddle screenWidth * heightCardRatio )
    in
    { x = toFloat screenWidth * xOffset - width_ / 2
    , y = toFloat screenHeight * yOffset - height_ / 2
    , width_ = width_
    , height_ = height_
    , rotation = Ui.radians 0
    }


calculatePlayAgainOrPassPosition : Int -> Int -> GBPosition
calculatePlayAgainOrPassPosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.5 - 180 / 2
    , y = toFloat screenHeight * 0.4 - 20 / 2
    , width_ = 180
    , height_ = 20
    , rotation = Ui.radians 0
    }


positionOpponent : Int -> FPlayer -> OpponentDisposition -> PositionedPlayer
positionOpponent screenWidth player opponentDisposition =
    let
        cardWidth : Float
        cardWidth =
            if List.length player.tableHand > 4 then
                (cardsPanelWidth - (spaceBetweenEachCard * (toFloat (List.length player.tableHand) - 1))) / toFloat (List.length player.tableHand)

            else
                cardsPanelWidth / 5

        cardsPanelWidth : Float
        cardsPanelWidth =
            toFloat screenWidth / 4

        leftSpace : Float
        leftSpace =
            20

        namePanelWidth : Float
        namePanelWidth =
            toFloat screenWidth / 8

        namePanelheight : Float
        namePanelheight =
            50

        rightSpace : Float
        rightSpace =
            20

        spaceBetweenEachCard : Float
        spaceBetweenEachCard =
            4
    in
    case opponentDisposition of
        LeftPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = leftSpace, y = 80 + 30 + toFloat i * (cardWidth + spaceBetweenEachCard), width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians (wantedSpinningRotationValue + pi / 2) } )) player.tableHand
            , namePosition = { x = leftSpace, y = 65, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }

        TopLeftPlayer ->
            let
                spaceBetweenNameAndCards : Float
                spaceBetweenNameAndCards =
                    8
            in
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = leftSpace + namePanelWidth + spaceBetweenNameAndCards + toFloat i * (cardWidth + spaceBetweenEachCard), y = 0, width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians wantedSpinningRotationValue } )) player.tableHand
            , namePosition = { x = leftSpace, y = 4, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }

        TopRightPlayer ->
            let
                panelWidth : Float
                panelWidth =
                    cardsPanelWidth + namePanelWidth
            in
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = toFloat screenWidth - cardWidth - toFloat i * (cardWidth + spaceBetweenEachCard) - rightSpace, y = 0, width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians wantedSpinningRotationValue } )) player.tableHand
            , namePosition = { x = toFloat screenWidth - panelWidth - rightSpace - 4, y = 4, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }

        RightPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = toFloat screenWidth - cardWidth - rightSpace, y = 90 + 30 + toFloat i * (cardWidth + spaceBetweenEachCard), width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians (wantedSpinningRotationValue + 3 * pi / 2) } )) player.tableHand
            , namePosition = { x = toFloat screenWidth - namePanelWidth - rightSpace, y = 75, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }
