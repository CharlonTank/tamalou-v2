module Frontend exposing (app)

-- import Animator exposing (Animation, keyframes, set, step, scaleX, ms)

import Animator exposing (scaleY)
import Animator.Timeline as Timeline exposing (Timeline)
import Animator.Transition
import Animator.Value
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Delay
import Html.Attributes as HA
import Internal.Style2 exposing (toRadians)
import Json.Decode exposing (maybe)
import Lamdera exposing (SessionId)
import List.Extra
import Palette.Anim as PAnim
import Simple.Animation as SAnimation exposing (easeInOutQuad)
import Simple.Animation.Animated as SAnim
import Simple.Animation.Property as SP
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Task
import Time exposing (Posix)
import Types exposing (ActionFromGameToBackend(..), CardAnimation(..), CardClickMsg(..), Counter(..), DiscardPile, FGame(..), FGameInProgressStatus(..), FPlayer, FPlayerToPlayStatus(..), FTableHand, FrontendModel, FrontendMsg(..), GBPosition, GameDisposition(..), LookACardStatus(..), OpponentDisposition(..), OpponentsDisposition, PlayerAction(..), PositionedPlayer, Positions, Switch2CardsStatus(..), TamalouOwner, ToBackend(..), ToFrontend(..))
import Ui exposing (..)
import Ui.Anim as Anim exposing (Animated, onTimeline, scaleX)
import Ui.Events as Events
import Ui.Font as Font
import Ui.Input as Input
import Ui.Layout as Layout
import Ui.Prose as Prose exposing (paragraph)
import Ui.Shadow as Shadow
import Url
import Utils.Ui exposing (..)



-- cardFlip : a
-- cardFlip =
--     keyframes
--         [ SAnim.set [ SAnim.scaleX 1 ]
--         , SAnim.step (SAnim.ms 250) [ SAnim.scaleX 0 ]
--         , SAnim.step (SAnim.ms 250) [ SAnim.scaleX 1 ]
--         ]
-- type alias Css =
--     { hash : String
--     , keyframes : String
--     , transition : String
--     , props : List ( String, String )
--     }
-- test =
--     Anim.div
--         (Anim.transition (Anim.ms 200)
--             [ Anim.opacity <|
--                 if model.visible then
--                     1
--                 else
--                     0
--             ]
--         )
--         [ Html.Attributes.id "my-element" ]
--         [ Html.text "Hello!" ]
-- animationToUiAttributes : Anim.Animation -> List (Element.Attribute msg)
-- animationToUiAttributes animation =
--     -- Implement conversion logic here.
--     -- This example assumes you have a way to extract CSS properties from your Animation type.
--     let
--         cssProperties : Anim.Css
--         cssProperties =
--             Anim.toCss animation
--     in
--     [ htmlAttribute <| HA.style "animation" cssProperties.keyframes
--     , htmlAttribute <| HA.style "transition" cssProperties.transition
--     ]
--         ++ List.map (\( key, value ) -> htmlAttribute <| HA.style key value) cssProperties.props
-- test =
--     Anim.div
--         (Anim.loop
--             [ Anim.step (Anim.ms 200)
--                 [ Anim.opacity 1
--                 ]
--             , Anim.wait (Anim.ms 200)
--             , Anim.step (Anim.ms 200)
--                 [ Anim.opacity 0
--                 ]
--             ]
--         )
--         []
--         [ Element.layout [] <| text "Hello" ]


phoneRotateAnimation : SAnimation.Animation
phoneRotateAnimation =
    SAnimation.steps
        { options = [ SAnimation.loop ]
        , startAt = [ SP.rotate 0, customTransformOrigin "center" ]
        }
        [ SAnimation.step 500 [ SP.rotate 90, customTransformOrigin "center" ]
        , SAnimation.wait 300
        , SAnimation.step 200 [ SP.rotate 0, customTransformOrigin "center" ]
        , SAnimation.wait 300
        ]



-- firstHalfFlip : Animation
-- firstHalfFlip =
--     Animation.fromTo
--         { duration = 500
--         , options = []
--         }
--         -- Animation for the first half of the flip (0 to 90 degrees)
--         -- Transition.transform : Millis -> List Option -> Property !!!!!!!!!!!!!!!!
--         -- Transition.transform =
--         --     property "transform"
--         -- this is no wrong:
--         Transition.transform
--         1000
--         [ P.rotateY 0, Transition.rotateY 90 ]
-- [ Transition.transform "rotateY(0deg)" ] -- THIS IS WRONG
-- [ Transition.transform "rotateY(90deg)" ] -- THIS IS WRONG
-- Animation for the second half of the flip (-90 to 0 degrees)
-- secondHalfFlip : Animation
-- secondHalfFlip =
--     Animation.fromTo
--         { duration = 500
--         , options = []
--         }
--         [ Transition.transform "rotateY(-90deg)" ]
--         [ Transition.transform "rotateY(0deg)" ]
-- Combined flip animation using steps
-- import Simple.Animation as Animation
-- import Simple.Animation.Property as P
-- cardFaceUpAnimation : SAnimation.Animation
-- cardFaceUpAnimation =
--     SAnimation.steps
--         { options = []
--         , startAt = [ SP.rotate 0, customTransformOrigin "center" ]
--         }
--         [ SAnimation.step 500 [ SP.rotate 180, customTransformOrigin "center" ]
--         ]


customTransformOrigin : String -> SP.Property
customTransformOrigin origin =
    SP.property "transform-origin" origin


minimalistPhoneWithHint : Svg FrontendMsg
minimalistPhoneWithHint =
    Svg.svg [ SvgA.viewBox "0 0 100 100" ]
        [ SAnim.svg { class = SvgA.class } Svg.g phoneRotateAnimation [] [ phoneSvg ]
        ]


phoneSvg : Svg FrontendMsg
phoneSvg =
    Svg.g
        [ SvgA.width "50"
        , SvgA.height "50"
        ]
        [ centeredRect [ SvgA.fill "black" ] 40 30 3
        , centeredRect [ SvgA.fill "grey" ] 41 31 2
        , Svg.circle [ SvgA.cx "50", SvgA.cy "31.6", SvgA.r "0.4", SvgA.fill "black" ] []
        , Svg.rect [ SvgA.x "39.8", SvgA.y "35", SvgA.width "0.5", SvgA.height "3", SvgA.rx "0.5", SvgA.ry "0.5", SvgA.fill "grey" ] []
        ]


centeredRect : List (Svg.Attribute FrontendMsg) -> Int -> Int -> Int -> Svg FrontendMsg
centeredRect attributes x y radius =
    let
        rectHeight : String
        rectHeight =
            (100 - y * 2) |> String.fromInt

        rectWidth : String
        rectWidth =
            (100 - x * 2) |> String.fromInt

        xInt : String
        xInt =
            String.fromInt x

        yInt : String
        yInt =
            String.fromInt y
    in
    Svg.rect
        (attributes
            ++ [ SvgA.x xInt
               , SvgA.width rectWidth
               , SvgA.y yInt
               , SvgA.height rectHeight
               , SvgA.rx (String.fromInt radius)
               , SvgA.ry (String.fromInt radius)
               ]
        )
        []


app : { init : Lamdera.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg ), onUrlChange : Url.Url -> FrontendMsg, onUrlRequest : UrlRequest -> FrontendMsg, subscriptions : FrontendModel -> Sub FrontendMsg, update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg ), updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg ), view : FrontendModel -> Browser.Document FrontendMsg }
app =
    Lamdera.frontend
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlClicked
        , subscriptions = subscriptions
        , update = update
        , updateFromBackend = updateFromBackend
        , view = view
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotWindowSize { height = h, width = w })
        , Browser.Events.onAnimationFrame Frame
        ]



-- updateMyAnimation : Posix ->
-- cardAnimator : Animator.Animator Model
-- cardAnimator =
--     Animator.animator
--         -- we tell the animator how to get the checked timeline using .checked
--         -- and we tell the animator how to update that timeline with updateChecked
--         |> Animator.watching .mario (\mario m -> { m | mario = mario })


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , device = Device Phone Landscape
      , fGame = FWaitingForPlayers []
      , clientId = Nothing
      , sessionId = Nothing
      , urlPath = url.path
      , errors = []
      , admin = False
      , viewPort = { height = 0, width = 0 }
      , ready = False
      , maybeName = Nothing
      , chatInput = ""
      , chat = []

      --   , cardAnim = CardNotFlipped
      , gameDisposition = NotCalculated
      , animationState = Anim.init
      , anim = False
      , posix = Time.millisToPosix 0
      , animDur = Nothing

      --   , nextStates = []
      --   , animations = []
      }
    , Task.perform
        (\v ->
            { height = round v.viewport.height
            , width = round v.viewport.width
            }
                |> GotWindowSize
        )
        Browser.Dom.getViewport
    )


scrollToBottom : String -> Cmd FrontendMsg
scrollToBottom elementId =
    Browser.Dom.getViewportOf elementId
        |> Task.andThen
            (\viewport ->
                Browser.Dom.setViewportOf elementId 0 viewport.scene.height
            )
        |> Task.attempt (always NoOpFrontendMsg)


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg ({ urlPath } as model) =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        GotWindowSize viewPort ->
            ( { model
                | device = classifyDevice viewPort
                , viewPort = viewPort
                , gameDisposition = calculateGameDisposition viewPort (fPlayersFromFGame model.fGame |> getOpponents model.sessionId) (getOwnedCards model.fGame)
              }
            , Cmd.none
            )

        ChangeCurrentPlayerNameFrontend newName ->
            ( { model | maybeName = Just newName }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChangeCurrentPlayerNameToBackend newName) )

        ImReadyFrontend ->
            ( { model
                | ready = True
                , chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, "Let's go I'm ready!" ) ]
              }
            , Lamdera.sendToBackend <| ActionFromGameToBackend urlPath ImReadyToBackend
            )

        ReStartGameFrontend fPlayer ->
            ( { model | ready = False }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ReStartGameToBackend fPlayer) )

        TamalouFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath TamalouToBackend )

        PowerIsUsedFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath PowerIsUsedToBackend )

        PowerPassFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath PowerIsNotUsedToBackend )

        ChangeChatInputFrontend newChatInput ->
            ( { model | chatInput = newChatInput }, Cmd.none )

        SendMessageFrontend ->
            ( { model | chatInput = "", chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, model.chatInput ) ] }
            , Cmd.batch
                [ Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (SendMessageToBackend model.chatInput)
                , scrollToBottom "chatty"
                ]
            )

        CardClickMsg cardClickMsg ->
            case cardClickMsg of
                DrawCardFromDeckFrontend ->
                    ( { model | anim = True }
                    , Cmd.batch
                        [ Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawFromDrawPileToBackend
                        , Delay.after 0 (UpdateFlip (CardFlipping FaceDown))
                        ]
                    )

                DrawFromDiscardPileFrontend ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawFromDiscardPileToBackend )

                DiscardCardFrontend ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DiscardCardInHandToBackend )

                CardClickReplacement cardIndex ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ReplaceCardInTableHandToBackend cardIndex) )

                DoubleCardFrontend cardIndex ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (DoubleCardInTableHandToBackend cardIndex) )

                LookAtCardFrontend cardIndex ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (LookAtCardInTableHandToBackend cardIndex) )

                ChooseOwnCardToSwitchFrontend cardIndex ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChooseOwnCardToSwitchToBackend cardIndex) )

                ChooseOpponentCardToSwitchFrontend sessionId cardIndex ->
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChooseOpponentCardToSwitchToBackend ( sessionId, cardIndex )) )

        UpdateFlip cardAnimation ->
            let
                cardInAnim =
                    (case model.fGame of
                        FGameInProgress _ _ _ _ _ (FYourTurn (FPlayerHasDraw fCard)) ->
                            Just fCard

                        _ ->
                            Nothing
                    )
                        |> Maybe.withDefault FaceDown
            in
            -- ( { model | cardAnim = cardAnimation }
            ( model
            , case cardAnimation of
                CardFlipping (FaceUp c) ->
                    Delay.after 500 (UpdateFlip (CardFlipped c))

                CardFlipping FaceDown ->
                    Delay.after 250 (UpdateFlip (CardFlipping cardInAnim))

                _ ->
                    Cmd.none
            )

        AnimMsg animMsg ->
            let
                ( newAnimationState, cmds ) =
                    Anim.update AnimMsg animMsg model.animationState
            in
            ( { model | animationState = newAnimationState }
            , cmds
            )

        Frame posix ->
            ( updateEveryTimelineOnFrame model posix, Cmd.none )

        UpdateFGamePostAnimationFrontend fGame playerAction ->
            -- based on the playerAction, let's update the model's gameDisposition
            ( { model
                | fGame = fGame
                , maybeName =
                    case model.maybeName of
                        Just _ ->
                            model.maybeName

                        Nothing ->
                            getMyName model.sessionId fGame

                -- , gameDisposition = calculateGameDispositionParts model.viewPort playerAction (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
                , gameDisposition = calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
              }
            , Cmd.none
            )



-- let's say we want to animate a card from one position to another
-- we need to know the start position and the end position
-- we need to know the total duration of the animation
-- calculateNewAnimationState : Posix -> Posix -> Int -> Timeline GBPosition -> Timeline GBPosition
-- calculateNewAnimationState lastTime newTime duration currentTimeline =
--     let
--         transition =
--             Animator.Transition.linear
--         movement : Timeline GBPosition -> GBPosition
--         movement state =
--             -- { x = Animator.Value.float currentTimeline calcMov |> Animator.Value.to |> Animator.Value.withTransition Animator.Transition.linear |> aaa
--             { x = Animator.Value.float currentTimeline calcMov |> Animator.Value.to |> Animator.Value.withTransition Animator.Transition.linear |> aaa
--             , y = Animator.Value.float (Animator.withTransition transition (Animator.Value.to newPosition.y)) state.y
--             , width_ = Animator.Value.float (Animator.withTransition transition (Animator.Value.to newPosition.width_)) state.width_
--             , height_ = Animator.Value.float (Animator.withTransition transition (Animator.Value.to newPosition.height_)) state.height_
--             , rotation = Animator.Value.float (Animator.withTransition transition (Animator.Value.to newPosition.rotation)) state.rotation
--             }
--         newTimeline =
--             Animator.Timeline.to (Animator.Duration.millis 3000) (movement model.positionTimeline) model.positionTimeline
--     in
--     { model | positionTimeline = newTimeline }
-- let
--     currentState : GBPosition
--     currentState =
--         Timeline.previous currentTimeline
--     endState : GBPosition
--     endState =
--         Timeline.current currentTimeline
--     newState : GBPosition
--     newState =
--         getGBPosition currentState endState lastTime newTime duration
--     newCurrentTimeline : Timeline GBPosition
--     newCurrentTimeline =
--         Timeline.to (Anim.ms 0) (Debug.log "NEWSTATE" newState) currentTimeline
-- in
-- Debug.log "new2" <| Timeline.to (Anim.ms <| toFloat <| duration - (Time.posixToMillis newTime - Time.posixToMillis lastTime)) endState newCurrentTimeline


getGBPosition : Timeline GBPosition -> GBPosition
getGBPosition timeline =
    { x = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .x)
    , y = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .y)
    , width_ = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .width_)
    , height_ = Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << .height_)
    , rotation = Ui.radians <| Animator.Value.float timeline (Animator.Value.withTransition Animator.Transition.standard << Animator.Value.to << toRadians << .rotation)
    }



-- animateOwnCard : FrontendModel -> Int -> FrontendModel
-- animateOwnCard model cardIndex =
--     case model.gameDisposition of
--         NotCalculated ->
--             model
--         Calculated positions ->
--             { model
--                 | animDur = Just 3000
--                 , gameDisposition =
--                     Calculated <|
--                         { positions
--                             | ownCardsDisposition =
--                                 positions.ownCardsDisposition
--                                     |> List.indexedMap
--                                         (\i ( card, timeline ) ->
--                                             if i == cardIndex then
--                                                 ( card, Timeline.to (Anim.ms animDuration) positions.discardPilePosition timeline )
--                                             else
--                                                 ( card, timeline )
--                                         )
--                         }
--             }
-- for now we only have a few animations, they reside in model.gameDisposition.ownCardsDisposition
-- to do that we need to apply to them : Timeline.update newPosix self
-- updateAnimations : FrontendModel -> Posix -> FrontendModel
-- updateAnimations model posix =
--     case model.gameDisposition of
--         NotCalculated ->
--             model
--         Calculated positions ->
--             let
--                 -- Update card positions and filter out finished animations
--                 updateCardPosition ( card, cardTimeline ) =
--                     ( card, Timeline.update posix cardTimeline )
--                 updatedPositions =
--                     { positions
--                         | ownCardsDisposition =
--                             positions.ownCardsDisposition
--                                 |> List.map updateCardPosition
--                     }
--                 -- Update other animations and filter out finished animations
--                 updatedAnimations =
--                     model.animations
--                         |> List.map (\timeline -> Timeline.update posix timeline)
--                         |> List.filter Timeline.isRunning
--             in
--             { model
--                 | gameDisposition = Calculated updatedPositions
--                 , animations = updatedAnimations
--             }
-- Define the updateCardPosition function outside to be used in multiple places


updateCardPosition : Posix -> ( FCard, Timeline GBPosition ) -> ( FCard, Timeline GBPosition )
updateCardPosition posix ( card, cardTimeline ) =
    ( card, Timeline.update posix cardTimeline )


updatePositionedPlayer : Posix -> PositionedPlayer -> PositionedPlayer
updatePositionedPlayer posix positionedPlayer =
    { positionedPlayer
        | positionedTableHand =
            positionedPlayer.positionedTableHand
                |> List.map (updateCardPosition posix)
    }


updateOpponentsDisposition : Posix -> OpponentsDisposition -> OpponentsDisposition
updateOpponentsDisposition posix opponentsDisposition =
    { opponentsDisposition
        | leftPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.leftPlayer
        , topLeftPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.topLeftPlayer
        , topRightPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.topRightPlayer
        , rightPlayer = Maybe.map (updatePositionedPlayer posix) opponentsDisposition.rightPlayer
    }


updateEveryTimelineOnFrame : FrontendModel -> Posix -> FrontendModel
updateEveryTimelineOnFrame model posix =
    case model.gameDisposition of
        NotCalculated ->
            model

        Calculated positions ->
            let
                updatedPositions =
                    { positions
                        | ownCardsDisposition =
                            positions.ownCardsDisposition
                                |> List.map (updateCardPosition posix)
                        , opponentsDisposition =
                            positions.opponentsDisposition
                                |> updateOpponentsDisposition posix
                        , cardsFromDrawPileMovingPositions =
                            positions.cardsFromDrawPileMovingPositions
                                |> List.map (Timeline.update posix)
                                |> List.filter Timeline.isRunning
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
                    }
            in
            { model
                | gameDisposition = Calculated updatedPositions
            }


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateAdminToFrontend errors ->
            ( { model | errors = errors }, Cmd.none )

        UpdateGameStatusToFrontend fGame maybePlayerAction ->
            case maybePlayerAction of
                Just playerAction ->
                    ( { model
                        | maybeName =
                            case model.maybeName of
                                Just _ ->
                                    model.maybeName

                                Nothing ->
                                    getMyName model.sessionId fGame
                      }
                        |> animatePlayerAction playerAction
                    , Delay.after (round animDuration) (UpdateFGamePostAnimationFrontend fGame playerAction)
                    )

                Nothing ->
                    ( { model
                        | fGame = fGame
                        , maybeName =
                            case model.maybeName of
                                Just _ ->
                                    model.maybeName

                                Nothing ->
                                    getMyName model.sessionId fGame
                        , gameDisposition = calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
                      }
                    , Cmd.none
                    )

        UpdateGameAndChatToFrontend ( fGame, chat ) ->
            ( { model
                | fGame = fGame
                , maybeName =
                    case model.maybeName of
                        Just _ ->
                            model.maybeName

                        Nothing ->
                            getMyName model.sessionId fGame
                , chat = chat
              }
            , scrollToBottom "chatty"
            )

        UpdateChatToFrontend chat ->
            ( { model | chat = chat }, scrollToBottom "chatty" )

        GotSessionIdAndClientIdToFrontend sessionId clientId ->
            if model.urlPath == "/admin" then
                ( { model | clientId = Just clientId, sessionId = Just sessionId, admin = True }
                , Lamdera.sendToBackend ConnectToAdminToBackend
                )

            else
                ( { model | clientId = Just clientId, sessionId = Just sessionId }
                , Lamdera.sendToBackend (ActionFromGameToBackend model.urlPath ConnectToBackend)
                )



-- type alias Positions =
--     { drawPilePosition : GBPosition
--     , drewCardPosition : GBPosition
--     , discardPilePosition : GBPosition
--     , tamalouButtonPosition : GBPosition
--     , playAgainOrPassPosition : GBPosition
--     , opponentsDisposition : OpponentsDisposition
--     , ownCardsDisposition : List ( FCard, Timeline GBPosition )
--     }
-- computeAnimation : FrontendModel -> playerAction -> List (Timeline GBPosition)
-- computeAnimation fModel playerAction =
--     case fModel.gameDisposition of
--         NotCalculated ->
--             []
--         Calculated positions ->
--             case playerAction of
--                 AnimationDoubleCardSuccess sessionId cardIndex ->
--                     let
--                         maybeCardToAnimate : Maybe GBPosition
--                         maybeCardToAnimate =
--                             positions.opponentsDisposition
--                                 |> findCardPosition sessionId cardIndex
--                     in
--                     case maybeCardToAnimate of
--                         Just cardToAnimate ->
--                             [ Timeline.to (Anim.ms animDuration) positions.discardPilePosition (Timeline.init cardToAnimate) ]
--                         Nothing ->
--                             let
--                                 maybeOwnCardToAnimate : Maybe (Timeline GBPosition)
--                                 maybeOwnCardToAnimate =
--                                     positions.ownCardsDisposition
--                                         |> List.Extra.getAt cardIndex
--                                         |> Maybe.map Tuple.second
--                             in
--                             case ( fModel.sessionId == Just sessionId, maybeOwnCardToAnimate ) of
--                                 ( True, Just cardToAnimate ) ->
--                                     [ Timeline.to (Anim.ms animDuration) positions.discardPilePosition cardToAnimate ]
--                                 _ ->
--                                     []
--                 -- for this we want to go the discard pile and comeback to the hand
--                 AnimationDoubleCardFailed sessionId cardIndex ->
--                     let
--                         maybeCardToAnimate : Maybe GBPosition
--                         maybeCardToAnimate =
--                             positions.opponentsDisposition
--                                 |> findCardPosition sessionId cardIndex
--                     in
--                     case maybeCardToAnimate of
--                         Just cardToAnimate ->
--                             let
--                                 -- Initialize the timeline with the original card position
--                                 initialTimeline =
--                                     Timeline.init cardToAnimate
--                                 -- Define the steps to go to the discard pile and come back
--                                 steps =
--                                     [ Timeline.transitionTo (Anim.ms 200) positions.discardPilePosition
--                                     , Timeline.wait (Anim.ms 200) -- Optional wait if needed
--                                     , Timeline.transitionTo (Anim.ms 200) cardToAnimate
--                                     ]
--                             in
--                             [ Timeline.queue steps initialTimeline ]
--                         Nothing ->
--                             let
--                                 maybeOwnCardToAnimate : Maybe (Timeline GBPosition)
--                                 maybeOwnCardToAnimate =
--                                     positions.ownCardsDisposition
--                                         |> List.Extra.getAt cardIndex
--                                         |> Maybe.map Tuple.second
--                             in
--                             case ( fModel.sessionId == Just sessionId, maybeOwnCardToAnimate ) of
--                                 ( True, Just ownCardTimeline ) ->
--                                     let
--                                         -- Define the steps to go to the discard pile and come back
--                                         steps =
--                                             [ Timeline.transitionTo (Anim.ms 200) positions.discardPilePosition
--                                             , Timeline.wait (Anim.ms 200) -- Optional wait if needed
--                                             , Timeline.transitionTo (Anim.ms 200) (Timeline.current ownCardTimeline)
--                                             ]
--                                     in
--                                     [ Timeline.queue steps ownCardTimeline ]
--                                 _ ->
--                                     []
--                 _ ->
--                     []


findCardPosition : SessionId -> Int -> OpponentsDisposition -> Maybe (Timeline GBPosition)
findCardPosition sessionId index { leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer } =
    let
        findInPlayer : PositionedPlayer -> Maybe (Timeline GBPosition)
        findInPlayer positionedPlayer =
            if sessionId == positionedPlayer.player.sessionId then
                positionedPlayer.positionedTableHand
                    |> List.Extra.getAt index
                    |> Maybe.map Tuple.second

            else
                Nothing
    in
    [ leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer ]
        |> List.filterMap (Maybe.andThen findInPlayer)
        |> List.head



-- computeOpponentAnimation : FrontendModel -> playerAction -> a -> b
-- computeOpponentAnimation fModel playerAction opponentDisposition =
-- case ( fModel.fGame, playerAction ) of
--     ( FGameInProgress _ _ _ _ players _, AnimationDoubleCardSuccess sessionId cardIndex ) ->
--         let
--             maybeCardToAnimate : Maybe FCard
--             maybeCardToAnimate =
--                 List.Extra.find (\player -> player.sessionId == sessionId) players
--                     |> Maybe.map .tableHand
--                     |> Maybe.andThen (List.Extra.getAt cardIndex)
--             -- if we don't find the player, let's see if the player is us
--         in
--         [ Timeline.to (Anim.ms animDuration) (positionDiff player.position) player.position ]
--     _ ->
--         []
-- For now, lets just do the AnimationDoubleCardSuccess, REMEMBER WILDCARD THE OTHER VARIANTS
--


animatePlayerAction : PlayerAction -> FrontendModel -> FrontendModel
animatePlayerAction playerAction fModel =
    case fModel.gameDisposition of
        Calculated positions ->
            case playerAction of
                AnimationDoubleCardSuccess sessionId cardIndex ->
                    let
                        maybeCardToAnimate : Maybe (Timeline GBPosition)
                        maybeCardToAnimate =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId cardIndex
                                |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
                    in
                    case maybeCardToAnimate of
                        Just cardToAnimate ->
                            { fModel
                                | gameDisposition =
                                    Calculated
                                        { positions
                                            | opponentsDisposition =
                                                positions.opponentsDisposition
                                                    |> applyAnimationToOpponent sessionId cardIndex cardToAnimate
                                        }
                            }

                        Nothing ->
                            let
                                maybeOwnCardToAnimate : Maybe (Timeline GBPosition)
                                maybeOwnCardToAnimate =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex
                                        |> Maybe.map Tuple.second
                                        |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
                            in
                            case ( fModel.sessionId == Just sessionId, maybeOwnCardToAnimate ) of
                                ( True, Just ownCardTimeline ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applyAnimationToOwnCard cardIndex ownCardTimeline
                                                }
                                    }

                                _ ->
                                    fModel

                -- For now, same same as AnimationDoubleCardSuccess
                AnimationDoubleCardFailed sessionId cardIndex ->
                    let
                        maybeCardToAnimate : Maybe (Timeline GBPosition)
                        maybeCardToAnimate =
                            positions.opponentsDisposition
                                |> findCardPosition sessionId cardIndex
                                |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
                    in
                    case maybeCardToAnimate of
                        Just cardToAnimate ->
                            { fModel
                                | gameDisposition =
                                    Calculated
                                        { positions
                                            | opponentsDisposition =
                                                positions.opponentsDisposition
                                                    |> applyAnimationToOpponent sessionId cardIndex cardToAnimate
                                        }
                            }

                        Nothing ->
                            let
                                maybeOwnCardToAnimate : Maybe (Timeline GBPosition)
                                maybeOwnCardToAnimate =
                                    positions.ownCardsDisposition
                                        |> List.Extra.getAt cardIndex
                                        |> Maybe.map Tuple.second
                                        |> Maybe.map (Timeline.to (Anim.ms animDuration) positions.discardPilePosition)
                            in
                            case ( fModel.sessionId == Just sessionId, maybeOwnCardToAnimate ) of
                                ( True, Just ownCardTimeline ) ->
                                    { fModel
                                        | gameDisposition =
                                            Calculated
                                                { positions
                                                    | ownCardsDisposition =
                                                        positions.ownCardsDisposition
                                                            |> applyAnimationToOwnCard cardIndex ownCardTimeline
                                                }
                                    }

                                _ ->
                                    fModel

                AnimationDrawCardFromDeck ->
                    { fModel
                        | gameDisposition =
                            Calculated
                                { positions
                                    | cardsFromDrawPileMovingPositions =
                                        (Timeline.init positions.drawPilePosition
                                            |> Timeline.to (Anim.ms animDuration) positions.drewCardPosition
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
                                                |> Timeline.to (Anim.ms animDuration) positions.drewCardPosition
                                            )
                                }
                    }

                AnimationSwitchCard ->
                    Debug.todo "branch 'AnimationSwitchCard' not implemented"

        -- _ ->
        --     fModel
        _ ->
            fModel


applyAnimationToOwnCard : Int -> Timeline GBPosition -> List ( FCard, Timeline GBPosition ) -> List ( FCard, Timeline GBPosition )
applyAnimationToOwnCard cardIndex newCardTimeline ownCardsDisposition =
    ownCardsDisposition
        |> List.Extra.updateIfIndex (\i -> i == cardIndex) (\( card, _ ) -> ( card, newCardTimeline ))


applyAnimationToOpponent : SessionId -> Int -> Timeline GBPosition -> OpponentsDisposition -> OpponentsDisposition
applyAnimationToOpponent sessionId cardIndex newCardTimeline { leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer } =
    let
        updateHand positionedPlayer =
            if sessionId == positionedPlayer.player.sessionId then
                Just { positionedPlayer | positionedTableHand = positionedPlayer.positionedTableHand |> List.Extra.updateIfIndex (\i -> i == cardIndex) (\( card, _ ) -> ( card, newCardTimeline )) }

            else
                Just positionedPlayer
    in
    { leftPlayer = leftPlayer |> Maybe.andThen updateHand
    , topLeftPlayer = topLeftPlayer |> Maybe.andThen updateHand
    , topRightPlayer = topRightPlayer |> Maybe.andThen updateHand
    , rightPlayer = rightPlayer |> Maybe.andThen updateHand
    }


fPlayersFromFGame : FGame -> List FPlayer
fPlayersFromFGame game =
    case game of
        FWaitingForPlayers players ->
            players

        FGameInProgress _ _ _ _ players _ ->
            players

        FGameEnded orderedPlayers ->
            List.map Tuple.first orderedPlayers

        FGameAlreadyStartedWithoutYou ->
            []


getOpponents : Maybe SessionId -> List FPlayer -> List FPlayer
getOpponents maybeSessionId players =
    players
        |> List.filter (\player -> maybeSessionId /= Just player.sessionId)


getOwnedCards : FGame -> List FCard
getOwnedCards fGame =
    case fGame of
        FGameInProgress _ hand _ _ _ _ ->
            hand

        _ ->
            []


getMyName : Maybe SessionId -> FGame -> Maybe String
getMyName maybeSessionId fGame =
    fPlayersFromFGame fGame
        |> List.Extra.find (\player -> maybeSessionId == Just player.sessionId)
        |> Maybe.map .name



-- {-| -}
-- layout :
--     { options : List Ui.Option
--     , toMsg : Msg -> msg
--     , breakpoints : Maybe (Ui.Responsive.Breakpoints label)
--     }
--     -> State
--     -> List (Attribute msg)
--     -> Element msg
--     -> Html.Html msg
-- layout opts state attrs els =
--     Two.renderLayout
--         { options =
--             case opts.breakpoints of
--                 Just (Two.Responsive breakpoints) ->
--                     breakpoints.breakpoints :: opts.options
--                 Nothing ->
--                     opts.options
--         , includeStaticStylesheet = True
--         }
--         state
--         (onAnimationStart opts.toMsg
--             :: onAnimationUnmount opts.toMsg
--             :: attrs
--         )
--         els


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tamalou!"
    , body =
        -- [ layout
        --     [ behindContent <| image [ width fill, height fill ] { source = "/background.png", description = "background" }
        --     , Font.size 12
        --     ]
        --   <|
        --     displayModel model
        -- ]
        [ Anim.layout
            { options = []
            , toMsg = AnimMsg
            , breakpoints = Nothing
            }
            model.animationState
            [ behindContent <| image [ width fill, height fill ] { source = "/background.png", description = "background" }
            , Font.size 12
            ]
            (displayModel model)
        ]
    }


displayModel : FrontendModel -> Element FrontendMsg
displayModel model =
    case model.gameDisposition of
        NotCalculated ->
            none

        Calculated positions ->
            column
                [ height fill, behindContent none ]
                [ if model.admin then
                    displayAdmin model

                  else
                    displayGame model positions
                ]


displayAdmin : FrontendModel -> Element FrontendMsg
displayAdmin model =
    List.map displayError model.errors
        |> column [ height fill, spacing 16 ]


displayError : String -> Element FrontendMsg
displayError error =
    Prose.paragraph [ width shrink ] [ text error ]


listfindAndRemove : (a -> Bool) -> List a -> ( Maybe a, List a )
listfindAndRemove predicate list =
    let
        ( found, rest ) =
            List.partition predicate list
    in
    case found of
        [ x ] ->
            ( Just x, rest )

        _ ->
            ( Nothing, list )


displayGameLobby : FrontendModel -> List FPlayer -> Element FrontendMsg
displayGameLobby fModel players =
    case fModel.sessionId of
        Just sessionId ->
            let
                ( maybeCurrentPlayer, _ ) =
                    listfindAndRemove (\player -> player.sessionId == sessionId) players
            in
            case maybeCurrentPlayer of
                Just currentPlayer ->
                    row
                        [ height fill ]
                        [ column [ width shrink, width <| portion 3, height fill, spacing 20 ]
                            [ el [ width shrink, centerX ] <| text "Tamalou!"
                            , let
                                label =
                                    Input.label "your-name-input" [ width shrink, centerX ] <| text "Your name"
                              in
                              Input.text [ width shrink, centerX, width <| px 200 ]
                                { label = label.id
                                , onChange = ChangeCurrentPlayerNameFrontend
                                , placeholder = Nothing
                                , text = fModel.maybeName |> Maybe.withDefault ""
                                }
                            , column [ width shrink, centerX, spacing 4 ] <|
                                let
                                    ( playersNotReady, playersReady ) =
                                        List.partition (\player -> not player.ready) players
                                in
                                [ if not <| List.isEmpty playersNotReady then
                                    el [ width shrink, centerX ] <| text <| "Players not ready:"

                                  else
                                    none
                                , column [ width shrink, spacing 8, centerX ] <| List.map displayPlayerName playersNotReady
                                , if not <| List.isEmpty playersReady then
                                    el [ width shrink, centerX ] <| text <| "Players ready:"

                                  else
                                    none
                                , column [ width shrink, spacing 8, centerX ] <| List.map displayPlayerName playersReady
                                ]
                            , if currentPlayer.ready then
                                el [ width shrink, centerX ] <| text "Waiting for other players to be ready"

                              else
                                el [ width shrink, centerX ] <| actionButton { label = text "I'm ready!", onPress = Just ImReadyFrontend }
                            ]
                        , displayChat fModel.viewPort.width fModel.viewPort.height fModel.chatInput fModel.chat
                        ]

                Nothing ->
                    column [ height fill ]
                        [ el [ width shrink, centerX, centerY ] <| text "Sorry, the game already started, you can't join"
                        , el [ width shrink, centerX, centerY ] <| text "Wait for the next game"
                        , column [ width shrink, centerX, spacing 4 ]
                            [ el [ width shrink, centerX ] <| text <| "Players playing"
                            , column [ width shrink, spacing 8, centerX ] <| List.map displayPlayerName players
                            ]
                        ]

        Nothing ->
            el [ width shrink, centerX, centerY ] <| text "-"


displayChat : Int -> Int -> String -> List ( String, String ) -> Element FrontendMsg
displayChat screenWidth screenHeight chatInput chat =
    column
        [ width shrink, width <| Ui.portion 4, height Ui.fill, Ui.spacing 8, Ui.paddingXY 12 12, Ui.background veryLightGrey, rounded 8 ]
        [ el [ width shrink, Ui.centerX ] <| Ui.text "Chat between players"
        , Ui.scrollable [] <| column [ Ui.spacing 6, height <| px <| screenHeight * 70 // 100, Ui.htmlAttribute <| HA.id "chatty" ] <| List.map (displayChatMessage screenWidth) chat
        , row [ Ui.alignBottom, Ui.spacing 4 ]
            [ Input.text [ width shrink, Ui.centerX, width <| px <| screenWidth * 40 // 100, Ui.alignLeft ]
                { label = Input.labelHidden "mess"
                , onChange = ChangeChatInputFrontend
                , placeholder = Nothing
                , text = chatInput
                }
            , el
                (centerX
                    :: (if chatInput == "" then
                            []

                        else
                            [ Events.onClick SendMessageFrontend
                            ]
                       )
                )
                (el [ width shrink, Ui.borderColor lightGrey, Ui.border 1, Ui.paddingXY 12 12, rounded 8 ] <| Ui.text "Send")
            ]
        ]


displayChatMessage : Int -> ( String, String ) -> Element FrontendMsg
displayChatMessage _ ( name, message ) =
    column
        [ spacing 2 ]
        [ row [ width shrink, width <| fill ] [ el [ Font.size 12 ] <| text name ]
        , row [ width shrink, width <| fill, paddingXY 12 0 ]
            [ el
                [ Font.size 16
                , background <|
                    if message == "Let's go I'm ready!" then
                        green

                    else
                        lightGrey
                , rounded 8
                , paddingXY 4 4
                ]
              <|
                Prose.paragraph [] [ text message ]
            ]
        ]


displayGame : FrontendModel -> Positions -> Element FrontendMsg
displayGame ({ viewPort } as model) { drawPilePosition, drewCardPosition, discardPilePosition, tamalouButtonPosition, playAgainOrPassPosition, opponentsDisposition, ownCardsDisposition, cardsFromDrawPileMovingPositions, cardFromDiscardPileMovingPositions } =
    case ( model.device.class, model.device.orientation ) of
        ( Phone, Portrait ) ->
            column [ Font.center, contentCenterY, height fill ]
                [ el [ centerX ] <| text "Rotate your phone 🚀"
                , el [ width shrink, centerX, height <| px 150, width <| px 150, move <| up 24 ] <| html minimalistPhoneWithHint
                ]

        _ ->
            el
                (height fill
                    :: List.map (elPlacedTimelined (displayFCard Nothing FaceDown)) cardsFromDrawPileMovingPositions
                )
            <|
                case model.fGame of
                    FWaitingForPlayers players ->
                        column
                            [ height fill ]
                            [ displayGameLobby model players ]

                    FGameInProgress _ hand _ _ players (FStartTimerRunning timer) ->
                        column
                            ([ height fill, Font.size 28 ] ++ displayOwnCards ownCardsDisposition Nothing Nothing)
                            [ column [ centerX, centerY, spacing 16 ]
                                [ el [ centerX, centerY ] <| text <| "Let's go! Remember your cards!"
                                , el [ centerX, centerY, Font.italic ] <| displayStartTimer timer
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FWaitingPlayerAction _)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            "It's "
                                                ++ fPlayer.name
                                                ++ "'s turn"
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill

                             --  , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerHasDraw _)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            fPlayer.name
                                                ++ " just drew a card"
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    displayFCard Nothing FaceDown
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerHasDiscard _)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            fPlayer.name
                                                ++ " can choose to use a power or not"
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drewCardPosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerLookACard lookACardStatus)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ (case lookACardStatus of
                                        ChooseCardToLook ->
                                            fPlayer.name ++ " is choosing a card to look at"

                                        LookingACard _ counter ->
                                            fPlayer.name ++ " is looking at a card: " ++ displayEndTimer counter
                                      )
                                        |> text
                                        |> middleText
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
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
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                -- A fix
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            fPlayer.name
                                                ++ " is choosing a card to switch"
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ (fPlayer.name ++ " is now choosing an opponent card to switch with")
                                        |> text
                                        |> middleText
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                    Just index

                                else
                                    Nothing
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                -- a fix avec le maybeindex
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                        let
                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            fPlayer.name
                                                ++ " changed a card with "
                                                ++ (opponent |> Maybe.map .name |> Maybe.withDefault "Anonymous")
                                                ++ "'s card: "
                                                ++ displayEndTimer counter
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                    Just index

                                else if (maybePlayer |> Maybe.map .sessionId) == Just opponentCard.sessionId then
                                    Just opponentCard.index

                                else
                                    Nothing

                            maybeOwnIndex : Maybe Int
                            maybeOwnIndex =
                                if model.sessionId == Just opponentCard.sessionId then
                                    Just opponentCard.index

                                else
                                    Nothing

                            opponent : Maybe FPlayer
                            opponent =
                                List.Extra.find (\player -> player.sessionId == opponentCard.sessionId) players
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                --fix avec le maybeindex
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                ++ displayOwnCards ownCardsDisposition Nothing maybeOwnIndex
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FWaitingPlayerAction maybePowerCard)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                -- elEmplacement viewPort.width <|
                                none

                            tamalouButton : Element FrontendMsg
                            tamalouButton =
                                case maybeTamalouOwner of
                                    Just _ ->
                                        none

                                    Nothing ->
                                        el [ Ui.centerX, Font.color blue, Font.italic ] <|
                                            el (actionBorder yellow ++ [ Events.onClick TamalouFrontend ]) <|
                                                text "Tamalou!"
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile True)
                             , elPlaced drewCardPosition <| drewCardColumn
                             , elPlaced tamalouButtonPosition <| tamalouButton
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile True maybePowerCard cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
                        let
                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            "You just drew a card"
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    displayFCard (Just DiscardCardFrontend) fCard
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition Nothing Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDiscard power)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            displayUsePowerOrPass : Element FrontendMsg
                            displayUsePowerOrPass =
                                row [ width shrink, centerX, spacing 8 ]
                                    [ actionButton { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend }
                                    , actionButton { label = text "Pass", onPress = Just PowerPassFrontend }
                                    ]

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                -- elEmplacement viewPort.width <|
                                none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             , elPlaced playAgainOrPassPosition <| displayUsePowerOrPass
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False (Just power) cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard ChooseCardToLook)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just LookAtCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text "Click on a card to look at it"
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard (LookingACard index counter))) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ middleText <|
                                        text <|
                                            displayEndTimer counter
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg (Just index)
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just ChooseOwnCardToSwitchFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ "Click on a card to switch" |> text |> middleText
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                        let
                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ text "You chose your card, now choose a card to switch with"
                                        |> middleText
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing True Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition Nothing (Just index)
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                        let
                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ text ("Remember! " ++ displayEndTimer counter)
                                        |> middleText
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    none

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                if (maybePlayer |> Maybe.map .sessionId) == Just opponentCard.sessionId then
                                    Just opponentCard.index

                                else
                                    Nothing
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                -- a fix avec le maybeindex
                                ++ displayAllOpponents maybeTamalouOwner Nothing True Nothing opponentsDisposition
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                                ++ displayOwnCards ownCardsDisposition Nothing (Just index)
                            )
                            []

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FEndTimerRunning timer) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ displayEndTimer timer
                                        |> text
                                        |> middleText
                                    , height fill
                                    ]
                                <|
                                    -- elEmplacement viewPort.width <|
                                    displayFCard Nothing FaceDown
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlaced drawPilePosition (displayDrawColumn drawPile False)
                             , elPlaced drewCardPosition <| drewCardColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                                ++ displayOwnCards ownCardsDisposition cardClickMsg Nothing
                                ++ displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile False Nothing cardFromDiscardPileMovingPositions
                            )
                            []

                    FGameEnded orderedPlayersAndRank ->
                        let
                            currentPlayerAndRank : Maybe ( FPlayer, Int )
                            currentPlayerAndRank =
                                List.Extra.find (\( player, _ ) -> Just player.sessionId == model.sessionId) orderedPlayersAndRank
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


medal : Int -> String
medal rank =
    case rank of
        1 ->
            "🥇"

        2 ->
            "🥈"

        3 ->
            "🥉"

        4 ->
            "🍪"

        _ ->
            "🤷\u{200D}♂️"


elEmplacement : Int -> Element FrontendMsg -> Element FrontendMsg
elEmplacement widthOfScreen cardToDisplay =
    el [ behindContent cardToDisplay ] <|
        image
            [ width <| px <| widthOfScreen // 7
            , rounded 8

            -- , height <| px <| widthOfScreen * 15 // 70
            -- , width shrink
            ]
            { source = "/emplacement.png", description = "Emplacement" }


displayDiscardCards : Int -> DiscardPile -> Bool -> Maybe Card.Power -> Element FrontendMsg
displayDiscardCards widthOfScreen discardPile canDrawCard maybePowerCard =
    case ( discardPile, canDrawCard, maybePowerCard ) of
        ( [], _, _ ) ->
            -- elEmplacement widthOfScreen <|
            none

        ( head :: _, True, Just power ) ->
            el
                [ width shrink
                , below <|
                    el [ Events.onClick Types.PowerIsUsedFrontend ] (text <| Card.powerToString power)
                ]
            <|
                elEmplacement widthOfScreen <|
                    displayFCard Nothing (FaceUp head)

        ( head :: _, True, Nothing ) ->
            -- Warning, here, we put True because it's not possible that the queen power has been used the turn before with someone with a valid tamalou && 2 players.
            -- We could pass the nb players and maybeTamalouOwner but it's not necessary for now, we come up with a better solution later.
            case Card.toPower True head of
                Just _ ->
                    -- elEmplacement widthOfScreen <|
                    displayFCard Nothing (FaceUp head)

                Nothing ->
                    -- elEmplacement widthOfScreen <|
                    displayFCard (Just DrawFromDiscardPileFrontend) (FaceUp head)

        ( head :: _, False, _ ) ->
            -- elEmplacement widthOfScreen <|
            displayFCard Nothing (FaceUp head)


displayDiscardCardsWithMaybeAnimation : GBPosition -> DiscardPile -> Bool -> Maybe Card.Power -> Maybe (Timeline GBPosition) -> List (Attribute FrontendMsg)
displayDiscardCardsWithMaybeAnimation discardPilePosition discardPile canDrawCard maybePowerCard maybeCardToAnimate =
    case maybeCardToAnimate of
        Just cardToAnimate ->
            -- During animations, all cards are displayed without the option to draw.
            case discardPile of
                first :: second :: _ ->
                    [ elPlacedTimelined (displayFCard Nothing (FaceUp first)) cardToAnimate, elPlaced discardPilePosition (displayFCard Nothing (FaceUp second)) ]

                first :: _ ->
                    [ elPlacedTimelined (displayFCard Nothing (FaceUp first)) cardToAnimate ]

                [] ->
                    []

        Nothing ->
            -- No animation is happening, so interactive options can be enabled based on game state.
            case ( discardPile, canDrawCard, maybePowerCard ) of
                ( [], _, _ ) ->
                    []

                ( first :: _, True, Just power ) ->
                    [ elPlaced discardPilePosition
                        (el
                            [ width shrink
                            , below <|
                                el [ Events.onClick Types.PowerIsUsedFrontend ] (text <| Card.powerToString power)
                            ]
                         <|
                            -- elEmplacement widthOfScreen <|
                            displayFCard Nothing (FaceUp first)
                        )
                    ]

                ( first :: _, True, Nothing ) ->
                    let
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


displayStartTimer : Counter -> Element FrontendMsg
displayStartTimer timer =
    text <|
        case timer of
            Five ->
                "5"

            Four ->
                "4"

            Three ->
                "3"

            Two ->
                "2"

            One ->
                "Start!"


displayEndTimer : Counter -> String
displayEndTimer timer =
    case timer of
        Five ->
            "5"

        Four ->
            "4"

        Three ->
            "3"

        Two ->
            "2"

        One ->
            "Time's up!"


displayPlayerName : FPlayer -> Element FrontendMsg
displayPlayerName player =
    let
        isReadyColor : Color
        isReadyColor =
            if player.ready then
                green

            else
                red
    in
    column
        [ width shrink, spacing 12, centerX, background isReadyColor, rounded 8, paddingXY 4 4 ]
        [ text <|
            case player.name of
                "" ->
                    "Anonymous"

                playerName ->
                    playerName
        ]


displayPlayerAndCards : ( FPlayer, Int ) -> Element FrontendMsg
displayPlayerAndCards ( player, rank ) =
    row
        [ spacing 12, centerX, rounded 8, paddingXY 12 12, background veryLightGrey, height <| px 64 ]
        [ text <| medal rank
        , el [ width <| px 250 ] <|
            text <|
                case player.name of
                    "" ->
                        "Anonymous"

                    playerName ->
                        playerName
        , el [ width shrink ] <| displayFCardsAtTheEnd player.tableHand
        , case player.score of
            Just score ->
                el [ width shrink, alignRight ] <| text <| String.fromInt score

            Nothing ->
                none
        ]


veryLightGrey : Color
veryLightGrey =
    rgb 240 240 240


lightGrey : Color
lightGrey =
    rgb 220 220 220



-- displayPlayerView : Int -> Maybe SessionId -> Maybe String -> DeviceClass -> List FPlayer -> FTableHand -> Maybe (Int -> CardClickMsg) -> Bool -> Maybe Int -> Element FrontendMsg
-- displayPlayerView screenWidth _ _ _ _ tableHand maybeCardClick _ maybeIndex =
--     row [ width shrink, alignBottom, centerX ]
--         [ displayOwnCards screenWidth tableHand maybeCardClick maybeIndex
--         ]
-- displayIndexedFCard : Maybe (Int -> CardClickMsg) -> Int -> FCard -> Element FrontendMsg
-- displayIndexedFCard maybeCardClickMsg index frontendCard =
--     image
--         ([ height fill, centerX ]
--             ++ (case maybeCardClickMsg of
--                     Just cardClickMsg ->
--                         cardActionBorder (cardClickMsg index)
--                     Nothing ->
--                         []
--                )
--         )
--     <|
--         case frontendCard of
--             FaceUp card ->
--                 { description = Card.toString card, source = "/cardImages/" ++ Card.toString card ++ ".png" }
--             FaceDown ->
--                 { description = "back", source = "/cardImages/BackCovers/Pomegranate.png" }


cardActionBorder : CardClickMsg -> List (Attribute FrontendMsg)
cardActionBorder cardClickMsg =
    let
        color : Color
        color =
            cardClickMsgToColor cardClickMsg
    in
    [ rounded 8
    , bigShadow color
    , Events.onClick <| CardClickMsg cardClickMsg
    , height fill
    , width fill
    ]


cardClickMsgToColor : CardClickMsg -> Color
cardClickMsgToColor cardClickMsg =
    case cardClickMsg of
        DrawCardFromDeckFrontend ->
            yellow

        DrawFromDiscardPileFrontend ->
            yellow

        DiscardCardFrontend ->
            yellow

        CardClickReplacement _ ->
            yellow

        DoubleCardFrontend _ ->
            blue

        LookAtCardFrontend _ ->
            green

        ChooseOwnCardToSwitchFrontend _ ->
            green

        ChooseOpponentCardToSwitchFrontend _ _ ->
            green


displayFCardsAtTheEnd : List FCard -> Element FrontendMsg
displayFCardsAtTheEnd cards =
    row [ width shrink, spacing 4, centerX, height fill ] (List.indexedMap displayFCardAtTheEnd cards)



-- sizeOfCard : Int -> Int
-- sizeOfCard screenWidth =
--     if screenWidth < 600 then
--         5 * screenWidth // 36
--     else if screenWidth < 800 then
--         4 * screenWidth // 36
--     else
--         3 * screenWidth // 36


displayOwnCards : List ( FCard, Timeline GBPosition ) -> Maybe (Int -> CardClickMsg) -> Maybe Int -> List (Attribute FrontendMsg)
displayOwnCards positionedCards maybeCardClickEvent maybeIndex =
    List.indexedMap (\i ( card, position ) -> elPlacedTimelined (displayFCardSized Nothing maybeCardClickEvent maybeIndex i card) position) positionedCards


displayFCard : Maybe CardClickMsg -> FCard -> Element FrontendMsg
displayFCard maybeCardClickMsg frontendCard =
    el
        -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes
        (case maybeCardClickMsg of
            Just cardClickMsg ->
                [ width fill, height fill ] ++ cardActionBorder cardClickMsg

            Nothing ->
                [ width fill, height fill ]
        )
    <|
        image [ height fill, width shrink, centerX ] <|
            case frontendCard of
                FaceUp card ->
                    { description = Card.toString card, source = "/cardImages/" ++ Card.toString card ++ ".png" }

                FaceDown ->
                    { description = "back", source = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardSized : Maybe Length -> Maybe (Int -> CardClickMsg) -> Maybe Int -> Int -> FCard -> Element FrontendMsg
displayFCardSized length maybeCardClickMsg maybeIndex index frontendCard =
    let
        attrs : List (Attribute FrontendMsg)
        attrs =
            Maybe.map (\cardClickMsg -> cardClickMsg index) maybeCardClickMsg |> Maybe.map cardActionBorder |> Maybe.withDefault []

        movedUp : List (Attribute FrontendMsg)
        movedUp =
            if maybeIndex == Just index then
                [ move <| up 12, bigShadow green ]

            else
                []
    in
    el [ height fill ] <|
        image
            -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes
            (width (Maybe.withDefault fill length) :: attrs ++ movedUp)
        <|
            case frontendCard of
                FaceUp card ->
                    { description = Card.toString card, source = "/cardImages/" ++ Card.toString card ++ ".png" }

                FaceDown ->
                    { description = "back", source = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardAtTheEnd : Int -> FCard -> Element FrontendMsg
displayFCardAtTheEnd =
    displayFCardSized (Just <| px 41) Nothing Nothing


actionBorder : Color -> List (Attribute FrontendMsg)
actionBorder color =
    [ rounded 8
    , background color
    , paddingXY 4 4
    , minimalistShadow
    ]


minimalistShadow : Ui.Attribute FrontendMsg
minimalistShadow =
    Shadow.shadows
        [ { x = 0
          , y = 0
          , size = 1
          , blur = 2
          , color = rgba 0 0 0 0.2
          }
        ]


bigShadow : Ui.Color -> Ui.Attribute FrontendMsg
bigShadow color =
    Shadow.shadows
        [ { blur = 8
          , color = color
          , x = 0
          , y = 0
          , size = 6
          }
        ]


yellow : Color
yellow =
    rgb 238 221 136


blue : Color
blue =
    rgb 0 68 221


green : Color
green =
    rgb 35 187 34


red : Color
red =
    rgb 224 38 15


actionButton : { label : Element FrontendMsg, onPress : Maybe FrontendMsg } -> Element FrontendMsg
actionButton { label, onPress } =
    el (actionBorder yellow ++ [ Events.onClick <| Maybe.withDefault NoOpFrontendMsg onPress ]) <| label


edges : { bottom : Int, left : Int, right : Int, top : Int }
edges =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


displayDrawColumn : List FCard -> Bool -> Element FrontendMsg
displayDrawColumn drawPile drawAllowed =
    -- elEmplacement screenWidth <|
    -- el [ width fill, height fill ] <|
    -- el [ width fill, height fill, inFront <| testAnimation cardAnim ] <|
    if drawAllowed then
        displayFCard (Just DrawCardFromDeckFrontend) FaceDown

    else if List.isEmpty drawPile then
        none

    else
        displayFCard Nothing FaceDown



-- displayOpponentRow : FPlayer -> Bool -> Bool -> Bool -> Maybe Int -> Element FrontendMsg
-- displayOpponentRow player isPlayerTurn isSwitchingCard isTamalouOwner maybeCardIndex =
--     let
--         attrsOwnTurn : List (Attribute FrontendMsg)
--         attrsOwnTurn =
--             if isPlayerTurn then
--                 [ Background.color yellow, Border.color yellow ]
--             else
--                 [ Border.color blue ]
--         switchingCardsAttrs : Maybe (Int -> CardClickMsg)
--         switchingCardsAttrs =
--             if isSwitchingCard && not isTamalouOwner then
--                 Just <| ChooseOpponentCardToSwitchFrontend player.sessionId
--             else
--                 Nothing
--     in
--     row
--         [ spacing 8 ]
--         [ column ([ Font.size 11, Border.width 1, Border.rounded 8, padding 4, spacing 4 ] ++ attrsOwnTurn) <| List.map (el [ centerX ] << text) (String.split " " player.name)
--         , row [ spacing 4, centerX, height fill ] <|
--             reverseIndexedMap
--                 (\i c -> el [ rotate (2 * pi / 2) ] <| displayFCardSized (px 50) switchingCardsAttrs maybeCardIndex i c)
--                 (player.tableHand |> List.reverse)
--         ]


reverseIndexedMap : (Int -> a -> b) -> List a -> List b
reverseIndexedMap f xs =
    List.indexedMap f xs |> List.reverse



-- displayRightOpponentColumn : FPlayer -> Bool -> Bool -> Bool -> Maybe Int -> Element FrontendMsg
-- displayRightOpponentColumn player isPlayerTurn isSwitchingCard isTamalouOwner maybeCardIndex =
--     let
--         attrsOwnTurn : List (Attribute FrontendMsg)
--         attrsOwnTurn =
--             if isPlayerTurn then
--                 [ Background.color yellow, Border.color yellow ]
--             else
--                 [ Border.color blue ]
--         switchingCardsMsg : Maybe (Int -> CardClickMsg)
--         switchingCardsMsg =
--             if isSwitchingCard && not isTamalouOwner then
--                 Just <| ChooseOpponentCardToSwitchFrontend player.sessionId
--             else
--                 Nothing
--     in
--     column
--         [ alignTop, height (fill |> Element.maximum 600) ]
--         [ el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4 ] ++ attrsOwnTurn) <| text player.name
--         , column [ spacing -22 ] <|
--             reverseIndexedMap
--                 (\i c -> el [ rotate (3 * pi / 2) ] <| displayFCardSized (px 50) switchingCardsMsg maybeCardIndex i c)
--                 (player.tableHand |> List.reverse)
--         ]
-- displayLeftOpponentColumn : FPlayer -> Bool -> Bool -> Bool -> Maybe Int -> Element FrontendMsg
-- displayLeftOpponentColumn player isPlayerTurn isSwitchingCard isTamalouOwner maybeCardIndex =
--     let
--         attrsOwnTurn : List (Attribute FrontendMsg)
--         attrsOwnTurn =
--             if isPlayerTurn then
--                 [ Background.color yellow, Border.color yellow ]
--             else
--                 [ Border.color blue ]
--         switchingCardsMsg : Maybe (Int -> CardClickMsg)
--         switchingCardsMsg =
--             if isSwitchingCard && not isTamalouOwner then
--                 Just <| ChooseOpponentCardToSwitchFrontend player.sessionId
--             else
--                 Nothing
--     in
--     column
--         [ alignTop, height fill ]
--         [ el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4, alignLeft ] ++ attrsOwnTurn) <| text player.name
--         , column [ spacing -22 ] <|
--             List.indexedMap
--                 (\i c -> el [ rotate (pi / 2) ] <| displayFCardSized (px 50) switchingCardsMsg maybeCardIndex i c)
--                 player.tableHand
--         ]
-- displayOwnName : Maybe String -> Bool -> Element FrontendMsg
-- displayOwnName maybeName isPlayerTurn =
--     let
--         attrs : List (Attr decorative FrontendMsg)
--         attrs =
--             if isPlayerTurn then
--                 [ Background.color yellow, Border.color yellow ]
--             else
--                 [ Border.color blue ]
--     in
--     el ([ Font.size 11, centerY, Border.width 1, Border.rounded 8, padding 4 ] ++ attrs) <|
--         case maybeName of
--             Just name ->
--                 text name
--             Nothing ->
--                 text "Anonymous"


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



-- type OpponentDisposition
--     = LeftPlayer
--     | TopLeftPlayer
--     | TopRightPlayer
--     | RightPlayer
-- type alias PositionedPlayer =
--     { player : FPlayer
--     , positionedTableHand : List ( FCard, Position )
--     }
-- type alias Position =
--     { x : Float
--     , y : Float
--     , width_ : Float
--     , height_ : Float
--     , rotation : Float
--     }


positionOpponent : Int -> FPlayer -> OpponentDisposition -> PositionedPlayer
positionOpponent screenWidth player opponentDisposition =
    let
        panelWidth : Float
        panelWidth =
            cardsPanelWidth + namePanelWidth

        cardsPanelWidth : Float
        cardsPanelWidth =
            toFloat screenWidth / 4

        namePanelWidth : Float
        namePanelWidth =
            toFloat screenWidth / 10

        namePanelheight : Float
        namePanelheight =
            50

        -- we need to reduce the width of the cards based on the number of cards in the hand if the number of cards exceeds 4.
        -- we don't want to exceed the width of panelWidth
        cardWidth : Float
        cardWidth =
            if List.length player.tableHand > 4 then
                (cardsPanelWidth - (spaceBetweenEachCard * (toFloat (List.length player.tableHand) - 1))) / toFloat (List.length player.tableHand)

            else
                cardsPanelWidth / 5

        spaceBetweenEachCard : Float
        spaceBetweenEachCard =
            4

        spaceBetweenNameAndCards : Float
        spaceBetweenNameAndCards =
            8

        leftSpace : Float
        leftSpace =
            20
    in
    case opponentDisposition of
        LeftPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = leftSpace, y = 80 + 30 + toFloat i * (cardWidth + spaceBetweenEachCard), width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians (wantedSpinningRotationValue + pi / 2) } )) player.tableHand
            , namePosition = { x = leftSpace, y = 80, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }

        TopLeftPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = leftSpace + namePanelWidth + spaceBetweenNameAndCards + toFloat i * (cardWidth + spaceBetweenEachCard), y = 0, width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians wantedSpinningRotationValue } )) player.tableHand
            , namePosition = { x = leftSpace, y = 0, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }

        TopRightPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = toFloat screenWidth - toFloat i * (cardWidth + spaceBetweenEachCard), y = 0, width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians wantedSpinningRotationValue } )) player.tableHand
            , namePosition = { x = toFloat screenWidth - panelWidth - (namePanelWidth / 2), y = 0, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }

        RightPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, Timeline.init { x = 0, y = toFloat i * (cardWidth + spaceBetweenEachCard), width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = Ui.radians (wantedSpinningRotationValue + 3 * pi / 2) } )) player.tableHand
            , namePosition = { x = toFloat screenWidth - (namePanelWidth / 2), y = 50, width_ = namePanelWidth, height_ = namePanelheight, rotation = Ui.radians 0 }
            }


displayOpponent : Maybe TamalouOwner -> Maybe SessionId -> Bool -> Maybe PositionedPlayer -> Maybe Int -> List (Attribute FrontendMsg)
displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard maybePositionedPlayer maybeCardIndex =
    case maybePositionedPlayer of
        Just positionedPlayer ->
            let
                switchingCardsMsg : Maybe (Int -> CardClickMsg)
                switchingCardsMsg =
                    if isSwitchingCard && not isTamalouOwner then
                        Just <| ChooseOpponentCardToSwitchFrontend positionedPlayer.player.sessionId

                    else
                        Nothing

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
            in
            displayOpponentName positionedPlayer.namePosition isPlayerTurn positionedPlayer.player.name :: List.indexedMap (\i ( c, position ) -> elPlacedTimelined (displayFCardSized Nothing switchingCardsMsg maybeCardIndex i c) position) positionedPlayer.positionedTableHand

        Nothing ->
            []


displayOpponentName : GBPosition -> Bool -> String -> Attribute FrontendMsg
displayOpponentName pos isPlayerTurn name =
    elPlaced pos
        (el
            -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes
            ([ width fill, height fill, padding 4, rounded 8, border 1, Font.size 11 ]
                ++ (if isPlayerTurn then
                        [ background yellow, borderColor yellow ]

                    else
                        [ borderColor blue ]
                   )
            )
         <|
            Prose.paragraph [ width shrink, spacing 4, Font.center, centerY ] <|
                [ text name ]
        )



-- let
--     isPlayerTurn : SessionId -> Bool
--     isPlayerTurn playerId =
--         maybeSessionId == Just playerId
--     isTamalouOwner : SessionId -> Bool
--     isTamalouOwner sessionId =
--         case maybeTamalouOwner of
--             Just tamalouOwner ->
--                 tamalouOwner.sessionId == sessionId
--             Nothing ->
--                 False
-- in
-- case opponentDisposition of
--     LeftPlayer -> none
--         -- displayLeftOpponentColumn player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex
--     TopLeftPlayer ->
--         el []
--         -- displayOpponentRow player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex
--     TopRightPlayer -> none
--         -- displayOpponentRow player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex
--     RightPlayer -> none
--         -- displayRightOpponentColumn player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex
--     _ ->
--         none
-- cardMoveAndFlip : GBPosition -> GBPosition -> CardAnimation -> Element FrontendMsg
-- cardMoveAndFlip oldPosition newPosition cardAnimation =
--     case cardAnimation of
--         CardFlipped card ->
--             none
--         -- displayFCard Nothing (FaceUp card)
--         CardNotFlipped ->
--             none
--         -- displayFCard (Just DrawCardFromDeckFrontend) FaceDown
--         CardFlipping fCard ->
--             Anim.el (cardFlip oldPosition newPosition) [ htmlAttribute <| HA.style "z-index" "10" ] <| displayFCard Nothing fCard
-- type alias Position =
--     { x : Float
--     , y : Float
--     , width_ : Float
--     , height_ : Float
--     , rotation : Float
--     }
-- cardFlip : GBPosition -> GBPosition -> SAnimation.Animation
-- cardFlip oldPosition newPosition =
--     -- fromTo : { duration : SAnimation.Millis, options : List SAnimation.Option } -> List SP.Property -> List SP.Property -> SAnimation.Animation
--     -- SAnimation.fromTo
--     --     { duration = 1000
--     --     , options = [ easeInOutQuad ]
--     --     }
--     --     [ SP.scaleXY 1 1, SP.x oldPosition.x, SP.y oldPosition.y, SP.rotate oldPosition.rotation ]
--     --     [ SP.scaleXY 0 1, SP.x newPosition.x, SP.y newPosition.y, SP.rotate newPosition.rotation ]
--     SAnimation.steps
--         { startAt = [ SP.scaleXY 1 1, SP.x oldPosition.x, SP.y oldPosition.y, SP.rotate oldPosition.rotation, SP.property "width" (String.fromFloat oldPosition.width_), SP.property "height" (String.fromFloat oldPosition.height_) ]
--         , options = [ easeInOutQuad ]
--         }
--         [ SAnimation.step 500 [ SP.scaleXY 0 1, SP.x newPosition.x, SP.y newPosition.y, SP.rotate newPosition.rotation, SP.property "width" (String.fromFloat newPosition.width_), SP.property "height" (String.fromFloat newPosition.height_) ]
--         , SAnimation.step 500 [ SP.scaleXY 1 1, SP.x newPosition.x, SP.y newPosition.y, SP.rotate newPosition.rotation, SP.property "width" (String.fromFloat newPosition.width_), SP.property "height" (String.fromFloat newPosition.height_) ]
--         ]
-- SAnimation.steps
--     { startAt = [ SP.scaleXY 1 1, SP.x 0 ]
--     , options = [ easeInOutQuad ]
--     }
--     [ SAnimation.step 2000 [ SP.scaleXY 0 1, SP.x 46 ]
--     , SAnimation.step 2000 [ SP.scaleXY 1 1, SP.x 92 ]
--     ]


cardWidthInMiddle : Int -> Float
cardWidthInMiddle widthOfScreen =
    toFloat widthOfScreen / 10


heightCardRatio : Float
heightCardRatio =
    380 / 250


elPlaced : GBPosition -> Element FrontendMsg -> Attribute FrontendMsg
elPlaced { x, y, width_, height_, rotation } =
    inFront << el [ move { offSet | y = round y, x = round x }, rotate rotation, width <| px <| round width_, height <| px <| round height_ ]


elPlacedTimelined : Element FrontendMsg -> Timeline GBPosition -> Attribute FrontendMsg
elPlacedTimelined content timeline =
    let
        { x, y, width_, height_, rotation } =
            getGBPosition timeline
    in
    inFront <|
        el
            [ move { offSet | y = round y, x = round x }
            , rotate rotation
            , width <| px <| round width_
            , height <| px <| round height_
            , Ui.htmlAttribute <| HA.style "z-index" "10"
            ]
            content



-- animGBPosition : GBPosition -> List Animated
-- animGBPosition { x, y, width_, height_, rotation } =
--     [ scaleX 10000, scaleY 1000, Anim.rotation 12 ]
-- type alias Position =
--     { x : Int
--     , y : Int
--     , z : Int
--     }
-- {-| -}
-- up : Int -> Position
-- up y =
--     { x = 0
--     , y = negate y
--     , z = 0
--     }
-- {-| -}
-- down : Int -> Position
-- down y =
--     { x = 0
--     , y = y
--     , z = 0
--     }
-- {-| -}
-- right : Int -> Position
-- right x =
--     { x = x
--     , y = 0
--     , z = 0
--     }
-- {-| -}
-- left : Int -> Position
-- left x =
--     { x = negate x
--     , y = 0
--     , z = 0
--     }
-- {-| -}
-- move : Position -> Attribute msg
-- move pos =
--     Two.style "translate"
--         ((String.fromInt pos.x ++ "px ")
--             ++ (String.fromInt pos.y ++ "px ")
--             ++ (String.fromInt pos.z ++ "px")
--         )


offSet : Position
offSet =
    { x = 0
    , y = 0
    , z = 0
    }


calculateDrawPilePosition : Int -> Int -> GBPosition
calculateDrawPilePosition screenWidth screenHeight =
    let
        width =
            cardWidthInMiddle screenWidth

        height =
            width * heightCardRatio
    in
    { x = toFloat screenWidth * 0.35 - width / 2
    , y = toFloat screenHeight * 0.35 - height / 2
    , width_ = width
    , height_ = height
    , rotation = Ui.radians 0
    }


calculateDrewCardPosition : Int -> Int -> GBPosition
calculateDrewCardPosition screenWidth screenHeight =
    let
        width =
            cardWidthInMiddle screenWidth

        height =
            width * heightCardRatio
    in
    { x = toFloat screenWidth * 0.5 - width / 2
    , y = toFloat screenHeight * 0.35 - height / 2
    , width_ = width
    , height_ = height
    , rotation = Ui.radians 0
    }


calculateDiscardPilePosition : Int -> Int -> GBPosition
calculateDiscardPilePosition screenWidth screenHeight =
    let
        width =
            cardWidthInMiddle screenWidth

        height =
            width * heightCardRatio
    in
    { x = toFloat screenWidth * 0.65 - width / 2
    , y = toFloat screenHeight * 0.35 - height / 2
    , width_ = width
    , height_ = height
    , rotation = Ui.radians 0
    }


calculateTamalouButtonPosition : Int -> Int -> GBPosition
calculateTamalouButtonPosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.5 - 65 / 2
    , y = toFloat screenHeight * 0.54 - 20 / 2
    , width_ = 65
    , height_ = 20
    , rotation = Ui.radians 0
    }


calculatePlayAgainOrPassPosition : Int -> Int -> GBPosition
calculatePlayAgainOrPassPosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.5 - 108 / 2
    , y = toFloat screenHeight * 0.65 - 20 / 2
    , width_ = 108
    , height_ = 20
    , rotation = Ui.radians 0
    }


calculateGameDisposition : { height : Int, width : Int } -> List FPlayer -> List FCard -> GameDisposition
calculateGameDisposition viewPort opponents ownCards =
    Calculated
        { drawPilePosition = calculateDrawPilePosition viewPort.width viewPort.height
        , cardsFromDrawPileMovingPositions = []
        , drewCardPosition = calculateDrewCardPosition viewPort.width viewPort.height
        , discardPilePosition = calculateDiscardPilePosition viewPort.width viewPort.height
        , cardFromDiscardPileMovingPositions = Nothing
        , tamalouButtonPosition = calculateTamalouButtonPosition viewPort.width viewPort.height
        , playAgainOrPassPosition = calculatePlayAgainOrPassPosition viewPort.width viewPort.height
        , opponentsDisposition = toOpponentsDisposition viewPort.width opponents
        , ownCardsDisposition = toOwnCardsDisposition viewPort ownCards
        }



-- type GameDispositionPart
--     = OpponentsDispositionPart OpponentDisposition Index
-- calculateGameDispositionParts : { height : Int, width : Int } -> PlayerAction -> List FPlayer -> List FCard -> GameDisposition
-- calculateGameDispositionParts viewPort playerAction opponents ownCards =
--     case playerAction of
--         AnimationDrawCardFromDeck sessionId ->
--             Calculated
--                 { drawPilePosition = calculateDrawPilePosition viewPort.width viewPort.height
--                 , drewCardPosition = calculateDrewCardPosition viewPort.width viewPort.height
--                 , discardPilePosition = calculateDiscardPilePosition viewPort.width viewPort.height
--                 , tamalouButtonPosition = calculateTamalouButtonPosition viewPort.width viewPort.height
--                 , playAgainOrPassPosition = calculatePlayAgainOrPassPosition viewPort.width viewPort.height
--                 , opponentsDisposition = toOpponentsDisposition viewPort.width opponents
--                 , ownCardsDisposition = toOwnCardsDisposition viewPort ownCards
--                 }
--         DrawFromDiscardPile ->
--             Calculated
--                 { drawPilePosition = calculateDrawPilePosition viewPort.width viewPort.height
--                 , drewCardPosition = calculateDrewCardPosition viewPort.width viewPort.height
--                 , discardPilePosition = calculateDiscardPilePosition viewPort.width viewPort.height
--                 , tamalouButtonPosition = calculateTamalouButtonPosition viewPort.width viewPort.height
--                 , playAgainOrPassPosition = calculatePlayAgainOrPassPosition viewPort.width viewPort.height
--                 , opponentsDisposition = toOpponentsDisposition viewPort.width opponents
--                 , ownCardsDisposition = toOwnCardsDisposition viewPort ownCards
--                 }
--         DiscardCard ->
--             Calculated
--                 { drawPilePosition = calculateDrawPilePosition viewPort.width viewPort.height
--                 , drewCardPosition = calculateDrewCardPosition viewPort.width viewPort.height
--                 , discardPilePosition = calculateDiscardPilePosition viewPort.width viewPort.height
--                 , tamalouButtonPosition = calculateTamalouButtonPosition viewPort.width viewPort.height
--                 , playAgainOrPassPosition = calculatePlayAgainOrPassPosition viewPort.width viewPort.height
--                 , opponentsDisposition = toOpponentsDisposition viewPort.width opponents
--                 , ownCardsDisposition = toOwnCardsDisposition viewPort ownCards
--                 }
--         Tamalou ->
--             Calculated
--                 { drawPilePosition = calculateDrawPilePosition viewPort.width viewPort.height
--                 , drewCardPosition = calculateDrewCardPosition viewPort.width viewPort.height
--                 , discardPilePosition = calculateDiscardPilePosition view


displayAllOpponents : Maybe TamalouOwner -> Maybe SessionId -> Bool -> Maybe Int -> OpponentsDisposition -> List (Attribute FrontendMsg)
displayAllOpponents maybeTamalouOwner maybeSessionId isSwitchingCard maybeCardIndex opponentsDisposition =
    [ displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.topLeftPlayer maybeCardIndex
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.topRightPlayer maybeCardIndex
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.leftPlayer maybeCardIndex
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.rightPlayer maybeCardIndex
    ]
        |> List.concat


toOwnCardsDisposition : { width : Int, height : Int } -> List FCard -> List ( FCard, Timeline GBPosition )
toOwnCardsDisposition viewPort ownCards =
    let
        cardPanel : Float
        cardPanel =
            (if totalCards == 1 then
                0.11

             else if totalCards == 2 then
                0.27

             else if totalCards == 3 then
                0.4

             else if totalCards == 4 then
                0.5

             else if totalCards == 5 then
                0.6

             else
                0.7
            )
                * toFloat viewPort.width

        totalCards : Int
        totalCards =
            List.length ownCards

        spaceBetweenEachCard : Float
        spaceBetweenEachCard =
            64 / (toFloat <| List.length ownCards)

        totalSpaceBetweenCards : Float
        totalSpaceBetweenCards =
            toFloat (totalCards - 1) * spaceBetweenEachCard

        cardWidth : Float
        cardWidth =
            (cardPanel - totalSpaceBetweenCards) / toFloat totalCards

        cardHeight : Float
        cardHeight =
            cardWidth * heightCardRatio

        startX : Float
        startX =
            (toFloat viewPort.width - (toFloat totalCards * cardWidth + totalSpaceBetweenCards)) / 2
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


middleText : Element FrontendMsg -> Attribute FrontendMsg
middleText =
    below << el [ width <| px 400, Font.center, padding 6, centerX ]



-- animateCardMove : FCard -> GBPosition -> GBPosition -> List Animated
-- animateCardMove card startPosition endPosition =
--     [ Anim.x endPosition.x
--     , Anim.y endPosition.y
--     , Anim.rotation (degrees 5) -- Adding a slight rotation for effect
--     , Anim.scale 0.95 -- Optionally scale down during movement
--     ]


animDuration : Float
animDuration =
    1000


wantedSpinningRotationValue : Float
wantedSpinningRotationValue =
    2 * pi
