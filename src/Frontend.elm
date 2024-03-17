module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Lamdera exposing (SessionId)
import List.Extra
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Task
import Types exposing (..)
import Url


grey : Color
grey =
    Element.rgb255 200 200 200


phoneRotateAnimation : Animation
phoneRotateAnimation =
    Animation.steps
        { startAt = [ P.rotate 0, customTransformOrigin "center" ]
        , options = [ Animation.loop ]
        }
        [ Animation.step 500 [ P.rotate 90, customTransformOrigin "center" ]
        , Animation.wait 300
        , Animation.step 200 [ P.rotate 0, customTransformOrigin "center" ]
        , Animation.wait 300
        ]


customTransformOrigin : String -> P.Property
customTransformOrigin origin =
    P.property "transform-origin" origin


minimalistPhoneWithHint : Svg FrontendMsg
minimalistPhoneWithHint =
    Svg.svg [ SvgA.viewBox "0 0 100 100" ]
        [ Animated.svg { class = SvgA.class } Svg.g phoneRotateAnimation [] [ phoneSvg ]
        ]


redSquare : Svg msg
redSquare =
    Svg.rect
        [ SvgA.width "50"
        , SvgA.height "50"
        , SvgA.fill "red"
        ]
        []


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


centeredRect : List (Svg.Attribute msg) -> Int -> Int -> Int -> Svg msg
centeredRect attributes x y radius =
    let
        xInt =
            String.fromInt x

        yInt =
            String.fromInt y

        rectWidth =
            (100 - x * 2) |> String.fromInt

        rectHeight =
            (100 - y * 2) |> String.fromInt
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


app : { init : Lamdera.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg ), view : FrontendModel -> Browser.Document FrontendMsg, update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg ), updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg ), subscriptions : FrontendModel -> Sub FrontendMsg, onUrlRequest : UrlRequest -> FrontendMsg, onUrlChange : Url.Url -> FrontendMsg }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Browser.Events.onResize (\w h -> GotWindowSize (classifyDevice { width = w, height = h }))


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , gameFrontend = FWaitingForPlayers []
      , clientId = Nothing
      , sessionId = Nothing
      , urlPath = url.path
      , device = Device Phone Landscape
      }
    , Task.perform
        (\v ->
            { height = round v.viewport.height
            , width = round v.viewport.width
            }
                |> classifyDevice
                |> GotWindowSize
        )
        Browser.Dom.getViewport
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg ({ urlPath } as model) =
    case msg of
        GotWindowSize device ->
            ( { model | device = device }
            , Cmd.none
            )

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

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        DrawCardFromDeckFrontend ->
            ( model, Lamdera.sendToBackend <| ToBackendActionFromGame urlPath DrawCardFromDrawPileToBackend )

        TamalouFrontend ->
            -- ( model, Debug.todo "Tamalou not implemented" )
            ( model, Cmd.none )

        DiscardCardFrontend ->
            ( model, Lamdera.sendToBackend <| ToBackendActionFromGame urlPath DiscardCardInHandToBackend )

        DrawCardFromDiscardPileFrontend ->
            ( model, Lamdera.sendToBackend <| ToBackendActionFromGame urlPath DrawCardFromDiscardPileToBackend )

        ReplaceCardInFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ToBackendActionFromGame urlPath (ReplaceCardInTableHandToBackend cardIndex) )

        DoubleCardFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ToBackendActionFromGame urlPath (DoubleCardInTableHandToBackend cardIndex) )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        GotSessionIdAndClientId sessionId clientId ->
            ( { model | clientId = Just clientId, sessionId = Just sessionId }
            , Lamdera.sendToBackend (ToBackendActionFromGame model.urlPath ConnectToBackend)
            )

        UpdateGame game ->
            ( { model | gameFrontend = game }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body = [ Element.layout [ width fill, height fill ] <| displayModel model ]
    }


displayModel : FrontendModel -> Element FrontendMsg
displayModel model =
    Element.column
        [ width fill, height fill, spacing 20, scrollbars ]
        [ displayGame model

        -- , displayGameDebug model
        ]


type CardClickEvent
    = CardClickReplacement
    | CardClickDouble


displayGame : FrontendModel -> Element FrontendMsg
displayGame model =
    case ( model.device.class, model.device.orientation ) of
        ( Phone, Portrait ) ->
            el [ width fill, height fill ] <|
                column [ Font.center, width fill, centerY ]
                    [ el [ width fill, centerX ] <| text "Rotate your phone 🚀"
                    , el [ centerX, height <| px 150, width <| px 150, moveUp 24 ] <| html minimalistPhoneWithHint
                    ]

        _ ->
            el [ width fill, height fill, Background.image "/background.jpeg", padding 12 ] <|
                case model.gameFrontend of
                    FWaitingForPlayers players ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            (text "Waiting for players" :: List.map displayPlayerDebug players)

                    FGameInProgress hand _ _ players (FTimerRunning timer) ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ displayTimer timer
                            , displayPlayerView model.sessionId model.device.class players hand Nothing
                            ]

                    FGameInProgress hand _ discardPile players (FPlayerToPlay _ FWaitingPlayerDraw) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ el [ Font.center, width fill ] <| text "Draw Pile"
                                    , elEmplacement <| displayCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ el [ Font.center, width fill ] <| text "Card drawn"
                                    , elEmplacement <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    ((el [ Font.center, width fill ] <| text "Discarded Pile")
                                        :: displayDiscardCards discardPile False
                                    )
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress hand _ discardPile players (FPlayerToPlay _ (FPlayerHasDraw _)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8, width fill ]
                                    [ el [ Font.center, width fill ] <| text "Draw Pile"
                                    , elEmplacement <| displayCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ el [ Font.center, width fill ] <| text "Card drawn"
                                    , elEmplacement <| displayCard Phone FaceDown
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8, width fill ]
                                    ((el [ Font.center, width fill ] <| text "Discarded Pile")
                                        :: displayDiscardCards discardPile False
                                    )
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress hand _ discardPile players (FYourTurn FWaitingPlayerDraw) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ el [ Font.center, width fill ] <| text "Draw Pile"
                                    , elEmplacement <| displayCard Phone FaceDown
                                    , Input.button [] { onPress = Just DrawCardFromDeckFrontend, label = text "Draw from drawPile" }
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ el [ Font.center, width fill ] <| text "Card drawn"
                                    , elEmplacement <| none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    ((el [ Font.center, width fill ] <| text "Discarded Pile")
                                        :: displayDiscardCards discardPile False
                                    )
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress hand _ discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
                        let
                            drawColumn =
                                column [ spacing 8 ]
                                    [ el [ Font.center, width fill ] <| text "Draw Pile"
                                    , elEmplacement <| displayCard Phone FaceDown
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ el [ Font.center, width fill ] <| text "Card drawn"
                                    , elEmplacement <| displayCard Phone fCard
                                    , Input.button [] { onPress = Just DiscardCardFrontend, label = text "Discard" }
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    ((el [ Font.center, width fill ] <| text "Discarded Pile")
                                        :: displayDiscardCards discardPile False
                                    )
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayPlayerView model.sessionId model.device.class players hand (Just CardClickReplacement)
                            ]

                    FGameEnded clientId ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ Element.text "Game ended"
                            , Element.text clientId
                            ]

                    FGameAlreadyStartedWithoutYou ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ Element.text "Sorry! The game already started without you, if you wanna play you can just go in a new url"
                            ]


elEmplacement : Element FrontendMsg -> Element FrontendMsg
elEmplacement cardToDisplay =
    el [ width <| px 144, height <| px 172, Background.image "/emplacement.png", Border.rounded 8 ] <| el [ width fill, height fill ] cardToDisplay



-- 168/202
-- = 0.8316831683168316
-- 150/180
-- = 0.8316831683168316
-- 140/168
-- = 0.8333333333333334
-- 144/172


displayDiscardCards : DiscardPile -> Bool -> List (Element FrontendMsg)
displayDiscardCards discardPile canDrawCard =
    case discardPile of
        [] ->
            [ elEmplacement <| none ]

        head :: _ ->
            [ el [ Events.onClick DrawCardFromDiscardPileFrontend ] <| elEmplacement <| displayCard Phone (FaceUp head)
            , if canDrawCard then
                Input.button [] { onPress = Just DrawCardFromDiscardPileFrontend, label = text "Draw the discardPile" }

              else
                none
            ]


displayTimer : Int -> Element FrontendMsg
displayTimer timer =
    text <|
        case timer of
            5 ->
                "5"

            4 ->
                "4"

            3 ->
                "3"

            2 ->
                "2"

            1 ->
                "1"

            0 ->
                "GOOOO"

            _ ->
                ""


displayPlayerDebug : FPlayer -> Element FrontendMsg
displayPlayerDebug player =
    column
        [ width fill, spacing 12 ]
        [ text player.name
        , row [ spacing 8 ] [ text "ClientId = ", text player.clientId ]
        , row [ spacing 8 ] [ text "SessionId = ", text player.sessionId ]
        ]


displayPlayerView : Maybe SessionId -> DeviceClass -> List FPlayer -> FTableHand -> Maybe CardClickEvent -> Element FrontendMsg
displayPlayerView _ deviceClass _ tableHand maybeCardClick =
    column [ width fill, alignBottom ]
        [ displayCards deviceClass tableHand maybeCardClick
        ]


displayGameDebug : FrontendModel -> Element FrontendMsg
displayGameDebug _ =
    column
        [ width fill, height fill, spacing 20, scrollbars ]
        [ text "Game debug"

        -- , text <| Debug.toString model
        ]


displayCards : DeviceClass -> List FCard -> Maybe CardClickEvent -> Element FrontendMsg
displayCards deviceClass cards maybeCardClickEvent =
    wrappedRow [ spacing 12, centerX ] (List.indexedMap (onClickCard maybeCardClickEvent (displayCard deviceClass)) cards)


onClickCard : Maybe CardClickEvent -> (FCard -> Element FrontendMsg) -> Int -> FCard -> Element FrontendMsg
onClickCard maybeCardClickEvent tag index card =
    case maybeCardClickEvent of
        Nothing ->
            tag card

        Just CardClickDouble ->
            Element.el [ Events.onClick (DoubleCardFrontend index) ] (tag card)

        Just CardClickReplacement ->
            Element.el [ Events.onClick (ReplaceCardInFrontend index) ] (tag card)


displayCard : DeviceClass -> FCard -> Element msg
displayCard deviceClass frontendCard =
    let
        cardWidth =
            case deviceClass of
                Phone ->
                    px 96

                Tablet ->
                    px 128

                Desktop ->
                    px 128

                BigDesktop ->
                    px 128
    in
    image [ Border.rounded 100, width cardWidth, centerX, centerY ] <|
        case frontendCard of
            FaceUp card ->
                { src = "/cardImages/" ++ Card.toString card ++ ".png", description = Card.toString card }

            FaceDown ->
                { src = "/cardImages/BackCovers/Pomegranate.png", description = "back" }
