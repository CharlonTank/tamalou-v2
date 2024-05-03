module Positioning.Ui exposing (..)

import Animator.Timeline exposing (Timeline)
import Card exposing (FCard(..), Power(..))
import Color exposing (..)
import Display.Common exposing (actionButton, displayFCard, displayFCardSized)
import Game exposing (DiscardPile, TamalouOwner)
import Html.Attributes as HA
import Lamdera exposing (SessionId)
import Player exposing (FPlayer)
import Positioning.Helpers exposing (offSet)
import Positioning.Positioning exposing (getGBPosition, moveUpBasedOnRotation)
import Positioning.Types exposing (GBPosition)
import Types exposing (..)
import Ui exposing (..)
import Ui.Font as Font
import Ui.Prose as Prose


elPlaced : GBPosition -> Element FrontendMsg -> Attribute FrontendMsg
elPlaced { x, y, width_, height_, rotation } =
    inFront << el [ move { offSet | x = round x, y = round y }, rotate rotation, width <| px <| round width_, height <| px <| round height_ ]


elPlacedTimelined : Element FrontendMsg -> Timeline GBPosition -> Attribute FrontendMsg
elPlacedTimelined content timeline =
    let
        { x, y, width_, height_, rotation } =
            getGBPosition timeline
    in
    inFront <|
        el
            [ move { offSet | x = round x, y = round y }
            , rotate rotation
            , width <| px <| round width_
            , height <| px <| round height_
            , Ui.htmlAttribute <| HA.style "z-index" "10"
            ]
            content


displayMiddleText : GBPosition -> String -> Attribute FrontendMsg
displayMiddleText drewCardPilePosition string =
    elPlaced drewCardPilePosition
        (el [ below (el [ width <| px 500, Font.center, padding 6, centerX ] <| text string) ] none)


displayAllOpponents : Maybe TamalouOwner -> Maybe SessionId -> Bool -> (Maybe FPlayer -> Maybe Int) -> OpponentsDisposition -> List (Attribute FrontendMsg)
displayAllOpponents maybeTamalouOwner maybeSessionId isSwitchingCard maybeHighlightCard opponentsDisposition =
    [ displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.topLeftPlayer (maybeHighlightCard <| Maybe.map .player opponentsDisposition.topLeftPlayer)
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.topRightPlayer (maybeHighlightCard <| Maybe.map .player opponentsDisposition.topRightPlayer)
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.leftPlayer (maybeHighlightCard <| Maybe.map .player opponentsDisposition.leftPlayer)
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.rightPlayer (maybeHighlightCard <| Maybe.map .player opponentsDisposition.rightPlayer)
    ]
        |> List.concat


displayOpponentName : GBPosition -> Bool -> String -> Attribute FrontendMsg
displayOpponentName pos isPlayerTurn name =
    elPlaced pos
        (el
            ([ width fill, height fill, rounded 8, border 1, Font.size 11 ]
                ++ (if isPlayerTurn then
                        [ background yellow, borderColor yellow ]

                    else
                        [ borderColor blue ]
                   )
            )
         <|
            Prose.paragraph [ width shrink, spacing 4, Font.center, centerY, centerX, padding 2, clip ] <|
                [ text name ]
        )


displayOpponent : Maybe TamalouOwner -> Maybe SessionId -> Bool -> Maybe PositionedPlayer -> Maybe Int -> List (Attribute FrontendMsg)
displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard maybePositionedPlayer maybeCardIndex =
    case maybePositionedPlayer of
        Just positionedPlayer ->
            let
                isPlayerTurn : Bool
                isPlayerTurn =
                    maybeSessionId == Just positionedPlayer.player.sessionId

                isTamalouOwner : Bool
                isTamalouOwner =
                    case maybeTamalouOwner of
                        Just tamalouOwner ->
                            tamalouOwner.sessionId == positionedPlayer.player.sessionId

                        Nothing ->
                            False

                switchingCardsMsg : Maybe (Int -> CardClickMsg)
                switchingCardsMsg =
                    if isSwitchingCard && not isTamalouOwner then
                        Just <| ChooseOpponentCardToSwitchFrontend positionedPlayer.player.sessionId

                    else
                        Nothing
            in
            displayOpponentName positionedPlayer.namePosition isPlayerTurn positionedPlayer.player.name
                :: List.indexedMap
                    (\i ( c, position ) ->
                        let
                            newPosition : Timeline GBPosition
                            newPosition =
                                case maybeCardIndex of
                                    Just index ->
                                        if index == i then
                                            moveUpBasedOnRotation position

                                        else
                                            position

                                    Nothing ->
                                        position
                        in
                        elPlacedTimelined (displayFCardSized Nothing switchingCardsMsg maybeCardIndex i c) newPosition
                    )
                    positionedPlayer.positionedTableHand

        Nothing ->
            []


displayDrawColumn : List FCard -> Bool -> Element FrontendMsg
displayDrawColumn drawPile drawAllowed =
    if drawAllowed then
        displayFCard (Just DrawCardFromDeckFrontend) FaceDown

    else if List.isEmpty drawPile then
        none

    else
        displayFCard Nothing FaceDown


displayOwnCards : List ( FCard, Timeline GBPosition ) -> Maybe (Int -> CardClickMsg) -> Maybe Int -> List (Attribute FrontendMsg)
displayOwnCards positionedCards maybeCardClickEvent maybeIndex =
    List.indexedMap (\i ( card, position ) -> elPlacedTimelined (displayFCardSized Nothing maybeCardClickEvent maybeIndex i card) position) positionedCards


displayDiscardCards : GBPosition -> DiscardPile -> Bool -> Maybe Card.Power -> Maybe (Timeline GBPosition) -> List (Attribute FrontendMsg)
displayDiscardCards discardPilePosition discardPile canDrawCard maybePowerCard maybeCardToAnimate =
    case maybeCardToAnimate of
        Just cardToAnimate ->
            case discardPile of
                [] ->
                    []

                first :: second :: _ ->
                    [ elPlacedTimelined (displayFCard Nothing (FaceUp first)) cardToAnimate, elPlaced discardPilePosition (displayFCard Nothing (FaceUp second)) ]

                first :: _ ->
                    [ elPlacedTimelined (displayFCard Nothing (FaceUp first)) cardToAnimate ]

        Nothing ->
            case ( discardPile, canDrawCard, maybePowerCard ) of
                ( [], _, _ ) ->
                    []

                ( first :: _, True, Just PlayAgain ) ->
                    [ elPlaced discardPilePosition (displayFCard Nothing (FaceUp first)) ]

                ( first :: _, True, Just Switch2Cards ) ->
                    [ elPlaced discardPilePosition
                        (el
                            [ inFront <|
                                el [ padding 16, move { offSet | y = 36 } ] <|
                                    actionButton { label = text <| Card.powerToString Switch2Cards, onPress = Just PowerIsUsedFrontend }
                            , height fill
                            ]
                         <|
                            displayFCard Nothing (FaceUp first)
                        )
                    ]

                ( first :: _, True, Just LookACard ) ->
                    [ elPlaced discardPilePosition
                        (el
                            [ inFront <|
                                el [ padding 16, move { offSet | y = 36 } ] <|
                                    actionButton { label = text <| Card.powerToString LookACard, onPress = Just PowerIsUsedFrontend }
                            , height fill
                            ]
                         <|
                            displayFCard Nothing (FaceUp first)
                        )
                    ]

                ( first :: _, True, Nothing ) ->
                    let
                        action : Maybe CardClickMsg
                        action =
                            case Card.toPower True first of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    Just DrawFromDiscardPileFrontend
                    in
                    [ elPlaced discardPilePosition (displayFCard action (FaceUp first)) ]

                ( first :: _, False, _ ) ->
                    [ elPlaced discardPilePosition (displayFCard Nothing (FaceUp first)) ]
