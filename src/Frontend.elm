module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (Card, FCard(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
import Lamdera exposing (SessionId)
import List.Extra
import Random
import Random.Extra
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
    Element.rgb255 0 0 0


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
    Browser.Events.onResize (\w h -> GotWindowSize { width = w, height = h })


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , gameFrontend = FWaitingForPlayers []
      , clientId = Nothing
      , sessionId = Nothing
      , urlPath = url.path
      , device = Device Phone Landscape
      , errors = []
      , admin = False
      , screenHeight = 0
      , screenWidth = 0
      , ready = False
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


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg ({ urlPath } as model) =
    case msg of
        GotWindowSize viewPort ->
            ( { model | device = classifyDevice viewPort, screenHeight = viewPort.height, screenWidth = viewPort.width }
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

        StartGameFrontend ->
            ( { model | ready = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath StartGameToBackend )

        DrawCardFromDeckFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawCardFromDrawPileToBackend )

        ChangeCurrentPlayerNameFrontend newName ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChangeCurrentPlayerNameToBackend newName) )

        TamalouFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath TamalouToBackend )

        DiscardCardFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DiscardCardInHandToBackend )

        DrawFromDiscardPileFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawFromDiscardPileToBackend )

        ReplaceCardInFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ReplaceCardInTableHandToBackend cardIndex) )

        DoubleCardFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (DoubleCardInTableHandToBackend cardIndex) )

        LookAtCardFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (LookAtCardInTableHandToBackend cardIndex) )

        PowerIsUsedFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath PowerIsUsedToBackend )

        PowerPassFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath PowerIsNotUsedToBackend )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateAdminToFrontend errors ->
            ( { model | errors = errors }, Cmd.none )

        GotSessionIdAndClientIdToFrontend sessionId clientId ->
            if model.urlPath == "/admin" then
                ( { model | clientId = Just clientId, sessionId = Just sessionId, admin = True }
                , Lamdera.sendToBackend ConnectToAdminToBackend
                )

            else
                ( { model | clientId = Just clientId, sessionId = Just sessionId }
                , Lamdera.sendToBackend (ActionFromGameToBackend model.urlPath ConnectToBackend)
                )

        UpdateGameToFrontend game ->
            ( { model | gameFrontend = game }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        safeAreaStyle =
            HA.style "padding"
                "env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left)"
    in
    { title = "Tamalou!"
    , body =
        [ Element.layout
            [ width Element.fill
            , height <| px model.screenHeight
            , Background.image "/background.png"
            , Background.color grey
            , htmlAttribute safeAreaStyle
            ]
          <|
            displayModel model
        ]
    }


displayModel : FrontendModel -> Element FrontendMsg
displayModel model =
    Element.column
        [ width fill, height fill, spacing 20, scrollbars ]
        [ if model.admin then
            displayAdmin model

          else
            displayGame model
        ]


type CardClickEvent
    = CardClickReplacement
    | CardClickDouble
    | LookThisCard


displayAdmin : FrontendModel -> Element FrontendMsg
displayAdmin model =
    List.map displayError model.errors
        |> column [ width fill, height fill ]


displayError : String -> Element FrontendMsg
displayError error =
    text error


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


displayGameLobby : Maybe SessionId -> List FPlayer -> Element FrontendMsg
displayGameLobby maybeSessionId players =
    case maybeSessionId of
        Nothing ->
            el [ centerX, centerY ] <| text "-"

        Just sessionId ->
            let
                ( maybeCurrentPlayer, otherPlayers ) =
                    listfindAndRemove (\player -> player.sessionId == sessionId) players
            in
            case maybeCurrentPlayer of
                Nothing ->
                    column [ width fill, height fill ]
                        [ el [ centerX, centerY ] <| text "Sorry, the game already started, you can't join"
                        , el [ centerX, centerY ] <| text "Wait for the next game"
                        , column [ centerX, spacing 4 ] <|
                            let
                                ( playersNotReady, playersReady ) =
                                    List.partition (\player -> not player.ready) players
                            in
                            [ if not <| List.isEmpty playersNotReady then
                                el [ centerX ] <| text <| "Players not ready:"

                              else
                                none
                            , column [ spacing 8, centerX ] <| List.map displayPlayer playersNotReady
                            , if not <| List.isEmpty playersReady then
                                el [ centerX ] <| text <| "Players ready:"

                              else
                                none
                            , column [ spacing 8, centerX ] <| List.map displayPlayer playersReady
                            ]
                        ]

                Just currentPlayer ->
                    column
                        [ width fill, height fill, spacing 20 ]
                        [ el [ centerX ] <| text "Tamalou!"
                        , Input.text [ centerX, width <| px 200 ]
                            { onChange = ChangeCurrentPlayerNameFrontend
                            , text = currentPlayer.name
                            , placeholder = Nothing
                            , label = Input.labelAbove [ centerX ] <| text "Your name"
                            }
                        , column [ centerX, spacing 4 ] <|
                            let
                                ( playersNotReady, playersReady ) =
                                    List.partition (\player -> not player.ready) players
                            in
                            [ if not <| List.isEmpty playersNotReady then
                                el [ centerX ] <| text <| "Players not ready:"

                              else
                                none
                            , column [ spacing 8, centerX ] <| List.map displayPlayer playersNotReady
                            , if not <| List.isEmpty playersReady then
                                el [ centerX ] <| text <| "Players ready:"

                              else
                                none
                            , column [ spacing 8, centerX ] <| List.map displayPlayer playersReady
                            ]
                        , if currentPlayer.ready then
                            el [ centerX ] <| text "Waiting for other players to be ready"

                          else
                            el [ centerX ] <| actionButton { onPress = Just StartGameFrontend, label = text "I'm ready!" }
                        ]


displayGame : FrontendModel -> Element FrontendMsg
displayGame ({ screenWidth } as model) =
    case ( model.device.class, model.device.orientation ) of
        ( Phone, Portrait ) ->
            el [ width fill, height fill ] <|
                column [ Font.center, width fill, centerY ]
                    [ el [ width fill, centerX ] <| text "Rotate your phone 🚀"
                    , el [ centerX, height <| px 150, width <| px 150, moveUp 24 ] <| html minimalistPhoneWithHint
                    ]

        _ ->
            el [ width fill, height fill, padding 12 ] <|
                case model.gameFrontend of
                    FWaitingForPlayers players ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ displayGameLobby model.sessionId players ]

                    FGameInProgress _ _ _ _ players (FStartTimerRunning Five) ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ el [ centerX, centerY ] <| displayStartTimer Five
                            , displayPlayerView model.sessionId model.device.class players [ FaceDown, FaceDown, FaceDown, FaceDown ] Nothing
                            ]

                    FGameInProgress _ hand _ _ players (FStartTimerRunning timer) ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ el [ centerX, centerY ] <| displayStartTimer timer
                            , displayPlayerView model.sessionId model.device.class players hand Nothing
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay _ (FWaitingPlayerAction _)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay _ (FPlayerHasDraw _)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <| displayFCard Phone FaceDown
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8, width fill ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand _ discardPile players (FYourTurn (FWaitingPlayerAction maybePowerCard)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        el
                                            ([ Events.onClick DrawCardFromDeckFrontend
                                             , centerX
                                             , centerY
                                             ]
                                                ++ actionBorder
                                            )
                                        <|
                                            displayFCard Phone FaceDown
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile True maybePowerCard)

                            tamalouButton =
                                el [ centerX ] <| actionButton { onPress = Just TamalouFrontend, label = text "Tamalou" }
                        in
                        column
                            [ width fill, height fill, spacing 4 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , case maybeTamalouOwner of
                                Just _ ->
                                    displayTamalou maybeTamalouOwner

                                Nothing ->
                                    tamalouButton
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
                        let
                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| el ([ centerX, centerY, Events.onClick DiscardCardFrontend ] ++ actionBorder) <| displayFCard Phone fCard
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand (Just CardClickReplacement)
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDiscard power)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False (Just power))

                            displayUsePowerOrPass =
                                row [ centerX, spacing 8 ]
                                    [ actionButton { onPress = Just PowerIsUsedFrontend, label = text <| Card.powerToString power }
                                    , actionButton { onPress = Just PowerPassFrontend, label = text "Pass" }
                                    ]
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayUsePowerOrPass
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay _ (FPlayerHasDiscard power)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay _ (FPlayerLookACard counter)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard Nothing)) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just LookThisCard

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , el [ centerX ] <| text "Click on a card to look at it"
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard (Just counter))) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just LookThisCard

                            drawColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8 ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , displayTamalou maybeTamalouOwner
                            , el [ centerX ] <| displayEndTimer counter
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FEndTimerRunning timer) ->
                        let
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            drawColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <|
                                        if List.isEmpty drawPile then
                                            none

                                        else
                                            displayFCard Phone FaceDown
                                    , none
                                    ]

                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <| displayFCard Phone FaceDown
                                    , none
                                    ]

                            discardPileColumn =
                                column [ spacing 8, width fill ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ spacing 16, centerX, centerY ]
                                [ drawColumn
                                , currentCardColumn
                                , discardPileColumn
                                ]
                            , el [ Font.center, width fill ] <| displayEndTimer timer
                            , displayTamalou maybeTamalouOwner
                            , displayPlayerView model.sessionId model.device.class players hand cardClickEvent
                            ]

                    FGameEnded orderedPlayers ->
                        let
                            rank =
                                orderedPlayers
                                    |> List.Extra.findIndex (\player -> Just player.sessionId == model.sessionId)
                                    |> Maybe.withDefault -1
                        in
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ el [ centerX ] <|
                                text <|
                                    case rank of
                                        0 ->
                                            "You win!🥇"

                                        1 ->
                                            "Maybe next time!🥈"

                                        2 ->
                                            "Luck is not on your side!🥉"

                                        3 ->
                                            "You lost! Here's a cookie 🍪"

                                        _ ->
                                            "You lost!🤷\u{200D}♂️"
                            , column [ centerX, spacing 4, width <| px <| (screenWidth * 80 // 100) ] <|
                                List.indexedMap (\i player -> displayPlayerAndCards i player) orderedPlayers
                            ]

                    FGameAlreadyStartedWithoutYou ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ text "Sorry! The game already started without you, if you wanna play you can just go in a new url"
                            ]


medal : Int -> String
medal rank =
    case rank of
        0 ->
            "🥇"

        1 ->
            "🥈"

        2 ->
            "🥉"

        3 ->
            "🍪"

        _ ->
            "🤷\u{200D}♂️"


displayTamalou : Maybe TamalouOwner -> Element FrontendMsg
displayTamalou maybeTamalouOwner =
    case maybeTamalouOwner of
        Just tamalouOwner ->
            column
                [ width fill, height fill, spacing 20 ]
                [ el [ Font.size <| 8 ] <| text "Last Turn!"
                , row [] <| List.map displayCard tamalouOwner.tableHand
                ]

        Nothing ->
            none


displayCard : Card -> Element FrontendMsg
displayCard card =
    el [ Background.image ("/cardImages/" ++ Card.toString card ++ ".png"), Border.rounded 8 ] none



-- 255 × 380
-- 182/122


elEmplacement : Int -> Element FrontendMsg -> Element FrontendMsg
elEmplacement widthOfScreen cardToDisplay =
    el
        [ width <| px <| widthOfScreen // 7
        , height <| px <| widthOfScreen * 15 // 70
        , Background.image "/emplacement.png"
        , Border.rounded 8
        ]
    <|
        el [ width fill, height fill ] cardToDisplay


displayDiscardCards : Int -> DiscardPile -> Bool -> Maybe Card.Power -> List (Element FrontendMsg)
displayDiscardCards widthOfScreen discardPile canDrawCard maybePowerCard =
    case ( discardPile, canDrawCard, maybePowerCard ) of
        ( [], _, _ ) ->
            [ elEmplacement widthOfScreen <| none ]

        ( head :: _, False, _ ) ->
            [ elEmplacement widthOfScreen <| displayFCard Phone (FaceUp head) ]

        ( head :: _, True, Nothing ) ->
            case Card.toPower head of
                Just _ ->
                    [ elEmplacement widthOfScreen <| displayFCard Phone (FaceUp head) ]

                Nothing ->
                    [ elEmplacement widthOfScreen <| el ([ centerX, centerY, Events.onClick DrawFromDiscardPileFrontend ] ++ actionBorder) <| displayFCard Phone (FaceUp head) ]

        ( head :: _, True, Just power ) ->
            [ elEmplacement widthOfScreen <| displayFCard Phone (FaceUp head)
            , Input.button [] { onPress = Just PowerIsUsedFrontend, label = text <| Card.powerToString power }
            ]


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
                "1"

            Zero ->
                "Start!"


displayEndTimer : Counter -> Element FrontendMsg
displayEndTimer timer =
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
                "1"

            Zero ->
                "Time's up!"


displayPlayer : FPlayer -> Element FrontendMsg
displayPlayer player =
    let
        isReadyColor =
            if player.ready then
                Element.rgb255 0 255 0

            else
                Element.rgb255 255 0 0
    in
    column
        [ spacing 12, centerX, Background.color isReadyColor, Border.rounded 8, paddingXY 2 2 ]
        [ text <|
            case player.name of
                "" ->
                    "Anonymous"

                playerName ->
                    playerName
        ]


displayPlayerAndCards : Int -> FPlayer -> Element FrontendMsg
displayPlayerAndCards rank player =
    row
        [ spacing 12, centerX, Border.rounded 8, paddingXY 2 2, Background.color veryLightGrey, width fill, height <| px 100 ]
        [ text <| medal rank
        , text <|
            case player.name of
                "" ->
                    "Anonymous"

                playerName ->
                    playerName
        , el [ width fill ] <| displayFCardsAtTheEnd player.tableHand
        ]


veryLightGrey : Color
veryLightGrey =
    Element.rgb255 240 240 240


displayPlayerView : Maybe SessionId -> DeviceClass -> List FPlayer -> FTableHand -> Maybe CardClickEvent -> Element FrontendMsg
displayPlayerView _ deviceClass _ tableHand maybeCardClick =
    column [ width fill, alignBottom ]
        [ displayFCards deviceClass tableHand maybeCardClick
        ]


displayGameDebug : FrontendModel -> Element FrontendMsg
displayGameDebug _ =
    column
        [ width fill, height fill, spacing 20, scrollbars ]
        [ text "Game debug"

        -- , text <| Debug.toString model
        ]


displayFCards : DeviceClass -> List FCard -> Maybe CardClickEvent -> Element FrontendMsg
displayFCards deviceClass cards maybeCardClickEvent =
    row [ spacing 4, centerX, width fill, paddingXY 128 0, height fill ] (List.indexedMap (onClickCard maybeCardClickEvent (displayFCard deviceClass)) cards)


displayFCardsAtTheEnd : List FCard -> Element FrontendMsg
displayFCardsAtTheEnd cards =
    row [ spacing 4, centerX, height fill ] (List.map displayFCardAtTheEnd cards)


onClickCard : Maybe CardClickEvent -> (FCard -> Element FrontendMsg) -> Int -> FCard -> Element FrontendMsg
onClickCard maybeCardClickEvent tag index card =
    case maybeCardClickEvent of
        Nothing ->
            tag card

        Just CardClickDouble ->
            Element.el [ Events.onClick <| DoubleCardFrontend index, Border.color blue, Border.rounded 8, Border.width 4, width fill ] (tag card)

        Just CardClickReplacement ->
            Element.el [ Events.onClick <| ReplaceCardInFrontend index, Border.color yellow, Border.rounded 8, Border.width 4, width fill ] (tag card)

        Just LookThisCard ->
            Element.el [ Events.onClick <| LookAtCardFrontend index, Border.color yellow, Border.rounded 8, Border.width 4, width fill ] (tag card)


displayFCard : DeviceClass -> FCard -> Element FrontendMsg
displayFCard deviceClass frontendCard =
    image [ Border.rounded 100, width fill, height fill, centerX, centerY ] <|
        case frontendCard of
            FaceUp card ->
                { src = "/cardImages/" ++ Card.toString card ++ ".png", description = Card.toString card }

            FaceDown ->
                { src = "/cardImages/BackCovers/Pomegranate.png", description = "back" }


displayFCardAtTheEnd : FCard -> Element FrontendMsg
displayFCardAtTheEnd frontendCard =
    image [ Border.rounded 100, height <| px 96, centerX, centerY ] <|
        case frontendCard of
            FaceUp card ->
                { src = "/cardImages/" ++ Card.toString card ++ ".png", description = Card.toString card }

            FaceDown ->
                { src = "/cardImages/BackCovers/Pomegranate.png", description = "back" }


actionBorder : List (Attribute FrontendMsg)
actionBorder =
    [ Border.rounded 8, Border.width 4, Border.color yellow ]


doubleActionBorder : List (Attribute FrontendMsg)
doubleActionBorder =
    [ Border.rounded 8, Border.width 4, Border.color blue ]


yellow : Color
yellow =
    Element.rgb255 255 255 0


blue : Color
blue =
    Element.rgb255 0 0 255


actionButton : { onPress : Maybe FrontendMsg, label : Element FrontendMsg } -> Element FrontendMsg
actionButton =
    Input.button actionBorder
