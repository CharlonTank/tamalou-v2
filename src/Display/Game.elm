module Display.Game exposing (..)

import Card exposing (FCard(..))
import Display.Common exposing (..)
import Display.Lobby as Lobby
import Game exposing (FGame(..), FGameInProgressStatus(..))
import Html.Attributes as HA
import List.Extra
import Palette.Anim as Anim
import Palette.Color exposing (..)
import Player exposing (FPlayer, FPlayerToPlayStatus(..), LookACardStatus(..), Switch2CardsStatus(..))
import Positioning.Ui exposing (..)
import Types exposing (..)
import Ui exposing (..)
import Ui.Events as Events
import Ui.Font as Font
import Utils.Ui exposing (DeviceClass(..), Orientation(..), actionBorder)


game : FrontendModel -> Positions -> Element FrontendMsg
game ({ sessionId, viewPort, alreadyInAction } as model) { drawPilePosition, cardsFromDrawPileMovingPositions, drewCardMovingPosition, middleTextPosition, discardPilePosition, cardFromDiscardPileMovingPositions, playAgainOrPassPosition, opponentsDisposition, ownCardsDisposition } =
    case ( model.device.class, model.device.orientation ) of
        ( Phone, Portrait ) ->
            column [ Font.center, contentCenterY, height fill ]
                [ el [ centerX ] <| text "Rotate your phone 🚀"
                , el [ width shrink, centerX, height <| px 150, width <| px 150, move <| up 24 ] <| html Anim.minimalistPhoneWithHint
                ]

        _ ->
            case model.fGame of
                Just fGame ->
                    el
                        (height fill
                            :: List.map (elPlacedTimelined (displayFCard Nothing FaceDown)) cardsFromDrawPileMovingPositions
                        )
                    <|
                        case fGame of
                            FWaitingForPlayers players ->
                                column
                                    [ height fill ]
                                    [ Lobby.display model players ]

                            FGameInProgress _ _ _ _ _ (FStartTimerRunning timer) ->
                                column
                                    ([ height fill, Font.size 28 ] ++ displayOwnCards ownCardsDisposition Nothing Nothing)
                                    [ column [ centerX, centerY, spacing 16 ]
                                        [ el [ centerX, centerY ] <| text <| "Let's go! Remember your cards!"
                                        , el [ centerX, centerY, Font.italic ] <| displayStartTimer timer
                                        ]
                                    ]

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FWaitingPlayerAction _)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition ("It's " ++ fPlayer.name ++ "'s turn")
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FPlayerHasDraw _)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , elPlacedTimelined (displayFCard Nothing FaceDown) drewCardMovingPosition
                                     , displayMiddleText middleTextPosition ("It's " ++ fPlayer.name ++ "'s turn")
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FPlayerHasDiscard _)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition (fPlayer.name ++ " can choose to use a power or not")
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FPlayerLookACard lookACardStatus)) ->
                                let
                                    maybeHighlightCard : Maybe FPlayer -> Maybe Int
                                    maybeHighlightCard maybePlayer =
                                        case lookACardStatus of
                                            ChooseCardToLook ->
                                                Nothing

                                            LookingACard index _ ->
                                                if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                                    Just index

                                                else
                                                    Nothing
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition
                                        (case lookACardStatus of
                                            ChooseCardToLook ->
                                                fPlayer.name ++ " is choosing a card to look at"

                                            LookingACard _ counter ->
                                                fPlayer.name ++ " is looking at a card: " ++ displayEndTimer counter
                                        )
                                     ]
                                        -- A fix
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False maybeHighlightCard opponentsDisposition
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition (fPlayer.name ++ " is choosing a card to switch")
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                                let
                                    maybeHighlightCard : Maybe FPlayer -> Maybe Int
                                    maybeHighlightCard maybePlayer =
                                        if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                            Just index

                                        else
                                            Nothing
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition (fPlayer.name ++ " is now choosing an opponent card to switch with")
                                     ]
                                        -- a fix avec le maybeindex
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False maybeHighlightCard opponentsDisposition
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                                let
                                    maybeHighlightCard : Maybe FPlayer -> Maybe Int
                                    maybeHighlightCard maybePlayer =
                                        if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                            Just index

                                        else if (maybePlayer |> Maybe.map .sessionId) == Just opponentCard.sessionId then
                                            Just opponentCard.index

                                        else
                                            Nothing

                                    maybeOwnIndex : Maybe Int
                                    maybeOwnIndex =
                                        if sessionId == Just opponentCard.sessionId then
                                            Just opponentCard.index

                                        else
                                            Nothing

                                    opponent : Maybe FPlayer
                                    opponent =
                                        List.Extra.find (\player -> player.sessionId == opponentCard.sessionId) players
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition (fPlayer.name ++ " changed a card with " ++ (opponent |> Maybe.map .name |> Maybe.withDefault "Anonymous") ++ "'s card: " ++ displayEndTimer counter)
                                     ]
                                        --fix avec le maybeindex
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False maybeHighlightCard opponentsDisposition
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                        ++ displayOwnCards ownCardsDisposition Nothing maybeOwnIndex
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FPlayerToPlay fPlayer (FPlayerDisplayTamalouFailure cardsToDisplay counter)) ->
                                let
                                    opponentsDispositionWithCardsToDisplay : OpponentsDisposition
                                    opponentsDispositionWithCardsToDisplay =
                                        { leftPlayer = updatePlayerCardsToDisplay opponentsDisposition.leftPlayer
                                        , topLeftPlayer = updatePlayerCardsToDisplay opponentsDisposition.topLeftPlayer
                                        , topRightPlayer = updatePlayerCardsToDisplay opponentsDisposition.topRightPlayer
                                        , rightPlayer = updatePlayerCardsToDisplay opponentsDisposition.rightPlayer
                                        }

                                    updatePlayerCardsToDisplay : Maybe PositionedPlayer -> Maybe PositionedPlayer
                                    updatePlayerCardsToDisplay maybePositionedPlayer =
                                        maybePositionedPlayer
                                            |> Maybe.map
                                                (\positionedPlayer ->
                                                    if positionedPlayer.player.sessionId == fPlayer.sessionId then
                                                        { positionedPlayer | positionedTableHand = List.map2 (\( _, timeline ) card -> ( FaceUp card, timeline )) positionedPlayer.positionedTableHand cardsToDisplay }

                                                    else
                                                        positionedPlayer
                                                )
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition ("Tamalou failed! " ++ displayEndTimer counter)
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False (always Nothing) opponentsDispositionWithCardsToDisplay
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FWaitingPlayerAction maybePowerCard)) ->
                                let
                                    tamalouButton : Element FrontendMsg
                                    tamalouButton =
                                        case maybeTamalouOwner of
                                            Just _ ->
                                                none

                                            Nothing ->
                                                el (actionBorder yellow ++ [ Events.onMouseUp TamalouFrontend, Font.color blue, Font.italic, centerX ]) <|
                                                    text "Tamalou!"
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile True)
                                     , elPlaced playAgainOrPassPosition <| tamalouButton
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile True maybePowerCard cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerHasDraw fCard)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , elPlacedTimelined (displayFCard (Just DiscardCardFrontend) fCard) drewCardMovingPosition
                                     , displayMiddleText middleTextPosition "You just drew a card"
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (Just CardClickReplacement) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerHasDiscard power)) ->
                                let
                                    displayUsePowerOrPass : Element FrontendMsg
                                    displayUsePowerOrPass =
                                        column [ spacing 12 ]
                                            [ actionButton { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend }
                                            , actionButton { label = text "Pass", onPress = Just PowerPassFrontend }
                                            ]
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , elPlaced playAgainOrPassPosition <| displayUsePowerOrPass
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False (Just power) cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerLookACard ChooseCardToLook)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition "Click on a card to look at it"
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (Just LookAtCardFrontend) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerLookACard (LookingACard index counter))) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition ("Remember! " ++ displayEndTimer counter)
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) (Just index)
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition "Click on a card to switch"
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (Just ChooseOwnCardToSwitchFrontend) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition "You chose your card, now choose a card to switch with"
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing True (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition Nothing (Just index)
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                                let
                                    maybeHighlightCard : Maybe FPlayer -> Maybe Int
                                    maybeHighlightCard maybePlayer =
                                        if (maybePlayer |> Maybe.map .sessionId) == Just opponentCard.sessionId then
                                            Just opponentCard.index

                                        else
                                            Nothing
                                in
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition ("Remember! " ++ displayEndTimer counter)
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing True maybeHighlightCard opponentsDisposition
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                        ++ displayOwnCards ownCardsDisposition Nothing (Just index)
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FYourTurn (FPlayerDisplayTamalouFailure _ counter)) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition ("Tamalou failed! " ++ displayEndTimer counter)
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameInProgress maybeTamalouOwner _ drawPile discardPile _ (FEndTimerRunning timer) ->
                                column
                                    ([ width <| px <| viewPort.width - 14
                                     , htmlAttribute (HA.style "user-select" "none")
                                     , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                                     , displayMiddleText middleTextPosition (displayEndTimer timer)
                                     ]
                                        ++ displayAllOpponents maybeTamalouOwner Nothing False (always Nothing) opponentsDisposition
                                        ++ displayOwnCards ownCardsDisposition (doubleCardClickMsg sessionId maybeTamalouOwner discardPile alreadyInAction) Nothing
                                        ++ displayDiscardCards discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                    )
                                    []

                            FGameEnded orderedPlayersAndRank ->
                                let
                                    currentPlayerAndRank : Maybe ( FPlayer, Int )
                                    currentPlayerAndRank =
                                        List.Extra.find (\( player, _ ) -> Just player.sessionId == sessionId) orderedPlayersAndRank
                                in
                                column
                                    [ spacing 12, padding 12 ]
                                    [ el [ width shrink, centerX ] <|
                                        text <|
                                            case currentPlayerAndRank of
                                                Just ( _, 1 ) ->
                                                    "You win!🥇"

                                                Just ( _, 2 ) ->
                                                    "Maybe next time!🥈"

                                                Just ( _, 3 ) ->
                                                    "Luck is not on your side!🥉"

                                                Just ( _, 4 ) ->
                                                    "You lost! Here's a cookie 🍪"

                                                Just _ ->
                                                    "You lost!🤷\u{200D}♂️"

                                                Nothing ->
                                                    "Game ended!"
                                    , column [ width shrink, centerX, spacing 4, width <| px <| (viewPort.width * 80 // 100) ] <|
                                        List.map (\player -> displayPlayerAndCards player) orderedPlayersAndRank
                                    , el [ width shrink, centerX ] <| actionButton { label = text "Play again!", onPress = Just (ReStartGameFrontend (currentPlayerAndRank |> Maybe.map Tuple.first)) }
                                    ]

                            FGameAlreadyStartedWithoutYou ->
                                column
                                    [ height fill, spacing 20 ]
                                    [ el [ width shrink, centerY, centerX ] <| text "Sorry! The game already started without you, if you wanna play you can just go in a new url"
                                    ]

                Nothing ->
                    column
                        [ height fill ]
                        [ text "Welcome to Tamalou! 🥳"
                        ]
