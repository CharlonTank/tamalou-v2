module Frontend exposing (app)

-- import Animator exposing (Animation, keyframes, set, step, scaleX, ms)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Delay
import Element exposing (Attr, Attribute, Color, Device, DeviceClass(..), Element, Length, Orientation(..), above, alignBottom, alignLeft, alignRight, alignTop, below, centerX, centerY, classifyDevice, column, el, fill, fillPortion, height, html, htmlAttribute, image, inFront, moveDown, moveRight, moveUp, none, onLeft, padding, paddingEach, paddingXY, paragraph, px, rotate, row, scrollbars, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
import Lamdera exposing (SessionId)
import List.Extra
import Palette.Anim as Anim
import Simple.Animation as SAnimation exposing (easeInOutQuad)
import Simple.Animation.Animated as SAnim
import Simple.Animation.Property as SP
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Task
import Types exposing (ActionFromGameToBackend(..), CardAnimation(..), CardClickMsg(..), Counter(..), DiscardPile, FGame(..), FGameInProgressStatus(..), FPlayer, FPlayerToPlayStatus(..), FTableHand, FrontendModel, FrontendMsg(..), GameDisposition(..), LookACardStatus(..), OpponentDisposition(..), OpponentsDisposition, Position, PositionedPlayer, Positions, Switch2CardsStatus(..), TamalouOwner, ToBackend(..), ToFrontend(..))
import Url



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
subscriptions _ =
    Browser.Events.onResize (\w h -> GotWindowSize { height = h, width = w })


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
      , cardAnim = CardNotFlipped
      , gameDisposition = NotCalculated
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
                , gameDisposition = calculateGameDisposition viewPort (fPlayersFromFGame model.fGame |> getOpponents model.sessionId)
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
                    ( model, Cmd.batch [ Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawCardFromDrawPileToBackend, Delay.after 0 (UpdateFlip (CardFlipping FaceDown) Nothing) ] )

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

        UpdateFlip cardAnimation card ->
            ( { model | cardAnim = cardAnimation }
            , case ( cardAnimation, card ) of
                ( CardFlipping (FaceUp c), _ ) ->
                    Delay.after 250 (UpdateFlip (CardFlipped c) (Just c))

                ( CardFlipping FaceDown, _ ) ->
                    Delay.after 250 (UpdateFlip (CardFlipping FaceDown) Nothing)

                _ ->
                    Cmd.none
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateAdminToFrontend errors ->
            ( { model | errors = errors }, Cmd.none )

        UpdateGameStatusToFrontend fGame ->
            ( { model
                | fGame = fGame
                , maybeName =
                    case model.maybeName of
                        Just _ ->
                            model.maybeName

                        Nothing ->
                            getMyName model.sessionId fGame
                , gameDisposition = calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId)
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


getMyName : Maybe SessionId -> FGame -> Maybe String
getMyName maybeSessionId fGame =
    fPlayersFromFGame fGame
        |> List.Extra.find (\player -> maybeSessionId == Just player.sessionId)
        |> Maybe.map .name


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tamalou!"
    , body =
        [ Element.layout
            [ width <| px model.viewPort.width
            , height <| px model.viewPort.height
            , Background.image "/background.png"
            , Font.size 12
            ]
          <|
            displayModel model
        ]
    }


displayModel : FrontendModel -> Element FrontendMsg
displayModel model =
    case model.gameDisposition of
        NotCalculated ->
            none

        Calculated positions ->
            Element.column
                [ width fill, height fill ]
                [ if model.admin then
                    displayAdmin model

                  else
                    displayGame model positions
                ]


displayAdmin : FrontendModel -> Element FrontendMsg
displayAdmin model =
    List.map displayError model.errors
        |> column [ width fill, height fill, spacing 16 ]


displayError : String -> Element FrontendMsg
displayError error =
    paragraph [] [ text error ]


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
                        [ width fill, height fill ]
                        [ column [ width <| fillPortion 3, height fill, spacing 20 ]
                            [ el [ centerX ] <| text "Tamalou!"
                            , Input.text [ centerX, width <| px 200 ]
                                { label = Input.labelAbove [ centerX ] <| text "Your name"
                                , onChange = ChangeCurrentPlayerNameFrontend
                                , placeholder = Nothing
                                , text = fModel.maybeName |> Maybe.withDefault ""
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
                                , column [ spacing 8, centerX ] <| List.map displayPlayerName playersNotReady
                                , if not <| List.isEmpty playersReady then
                                    el [ centerX ] <| text <| "Players ready:"

                                  else
                                    none
                                , column [ spacing 8, centerX ] <| List.map displayPlayerName playersReady
                                ]
                            , if currentPlayer.ready then
                                el [ centerX ] <| text "Waiting for other players to be ready"

                              else
                                el [ centerX ] <| actionButton { label = text "I'm ready!", onPress = Just ImReadyFrontend }
                            ]
                        , displayChat fModel.viewPort.width fModel.viewPort.height fModel.chatInput fModel.chat
                        ]

                Nothing ->
                    column [ width fill, height fill ]
                        [ el [ centerX, centerY ] <| text "Sorry, the game already started, you can't join"
                        , el [ centerX, centerY ] <| text "Wait for the next game"
                        , column [ centerX, spacing 4 ]
                            [ el [ centerX ] <| text <| "Players playing"
                            , column [ spacing 8, centerX ] <| List.map displayPlayerName players
                            ]
                        ]

        Nothing ->
            el [ centerX, centerY ] <| text "-"


displayChat : Int -> Int -> String -> List ( String, String ) -> Element FrontendMsg
displayChat screenWidth screenHeight chatInput chat =
    column
        [ width <| fillPortion 4, height fill, spacing 8, paddingXY 12 12, Background.color veryLightGrey, Border.rounded 8 ]
        [ el [ centerX ] <| text "Chat between players"
        , column [ spacing 6, height <| px <| screenHeight * 70 // 100, scrollbars, htmlAttribute <| HA.id "chatty", width fill ] <| List.map (displayChatMessage screenWidth) chat
        , row [ alignBottom, spacing 4, width fill ]
            [ Input.text [ centerX, width <| px <| screenWidth * 40 // 100, alignLeft ]
                { label = Input.labelHidden "mess"
                , onChange = ChangeChatInputFrontend
                , placeholder = Nothing
                , text = chatInput
                }
            , Input.button [ centerX ]
                { label = el [ Border.color lightGrey, Border.width 1, paddingXY 12 12, Border.rounded 8 ] <| text "Send"
                , onPress =
                    if chatInput == "" then
                        Nothing

                    else
                        Just SendMessageFrontend
                }
            ]
        ]


displayChatMessage : Int -> ( String, String ) -> Element FrontendMsg
displayChatMessage _ ( name, message ) =
    column
        [ width fill, spacing 2 ]
        [ row [ width <| fill ] [ el [ width fill, Font.size 12 ] <| text name ]
        , row [ width <| fill, paddingXY 12 0 ]
            [ el
                [ width fill
                , Font.size 16
                , Background.color <|
                    if message == "Let's go I'm ready!" then
                        green

                    else
                        lightGrey
                , Border.rounded 8
                , paddingXY 4 4
                ]
              <|
                paragraph [ width fill ] [ text message ]
            ]
        ]


displayGame : FrontendModel -> Positions -> Element FrontendMsg
displayGame ({ viewPort } as model) { drawPilePosition, drewCardPosition, discardPilePosition, tamalouButtonPosition, playAgainOrPassPosition, opponentsDisposition } =
    case ( model.device.class, model.device.orientation ) of
        ( Phone, Portrait ) ->
            column [ Font.center, width fill, centerY ]
                [ el [ width fill, centerX ] <| text "Rotate your phone 🚀"
                , el [ centerX, height <| px 150, width <| px 150, moveUp 24 ] <| html minimalistPhoneWithHint
                ]

        _ ->
            el [ width fill, height fill, scrollbars ] <|
                case model.fGame of
                    FWaitingForPlayers players ->
                        column
                            [ width fill, height fill, scrollbars, paddingXY 0 0 ]
                            [ displayGameLobby model players ]

                    FGameInProgress _ _ _ _ players (FStartTimerRunning Five) ->
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ el [ centerX, centerY ] <| displayStartTimer Five
                            , displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players [ FaceDown, FaceDown, FaceDown, FaceDown ] Nothing False Nothing
                            ]

                    FGameInProgress _ hand _ _ players (FStartTimerRunning timer) ->
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ el [ centerX, centerY ] <| displayStartTimer timer
                            , displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand Nothing False Nothing
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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| "It's " ++ fPlayer.name ++ "'s turn"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ row [ width fill, height fill ]
                                [ column [ width fill, spacing 12, height fill ]
                                    [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                ]
                            ]

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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| fPlayer.name ++ " just drew a card"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        displayFCard Nothing FaceDown
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                ]
                            ]

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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| fPlayer.name ++ " can choose to use a power or not"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drewCardPosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ row [ width fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                ]
                            ]

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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <|
                                        el [ centerX, padding 12 ] <|
                                            text <|
                                                case lookACardStatus of
                                                    ChooseCardToLook ->
                                                        fPlayer.name ++ " is choosing a card to look at"

                                                    LookingACard _ counter ->
                                                        fPlayer.name ++ " is looking at a card: " ++ displayEndTimer counter
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
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
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                -- A fix
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just DoubleCardFrontend

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| fPlayer.name ++ " is choosing a card to switch"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just DoubleCardFrontend

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| fPlayer.name ++ " is now choosing an opponent card to switch with"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
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
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                -- a fix avec le maybeindex
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                        let
                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| fPlayer.name ++ " changed a card with " ++ (opponent |> Maybe.map .name |> Maybe.withDefault "Anonymous") ++ "'s card: " ++ displayEndTimer counter
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
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
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                --fix avec le maybeindex
                                ++ displayAllOpponents maybeTamalouOwner (Just fPlayer.sessionId) False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand Nothing False maybeOwnIndex
                                ]
                            ]

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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile True maybePowerCard

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                elEmplacement viewPort.width <| none

                            tamalouButton : Element FrontendMsg
                            tamalouButton =
                                case maybeTamalouOwner of
                                    Just _ ->
                                        el [ centerX, paddingEach { edges | bottom = 16, top = 12 } ] none

                                    Nothing ->
                                        el [ Font.color blue, Font.italic ] <|
                                            Input.button (actionBorder yellow)
                                                { label = text "\"Tamalou!\"", onPress = Just TamalouFrontend }
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile True model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             , elPlacedByCenter tamalouButtonPosition <| tamalouButton
                             , inFront (cardMoveAndFlip drawPilePosition drewCardPosition model.cardAnim)
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
                        let
                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| "You just drew a card"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        displayFCard (Just DiscardCardFrontend) fCard
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             , inFront (cardMoveAndFlip drawPilePosition drewCardPosition model.cardAnim)
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand (Just CardClickReplacement) True Nothing
                                ]
                            ]

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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False (Just power)

                            displayUsePowerOrPass : Element FrontendMsg
                            displayUsePowerOrPass =
                                row [ centerX, spacing 8 ]
                                    [ actionButton { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend }
                                    , actionButton { label = text "Pass", onPress = Just PowerPassFrontend }
                                    ]

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                elEmplacement viewPort.width <| none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             , elPlacedByCenter playAgainOrPassPosition <| displayUsePowerOrPass
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard ChooseCardToLook)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just LookAtCardFrontend

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text "Click on a card to look at it"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard (LookingACard index counter))) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just DoubleCardFrontend

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| displayEndTimer counter
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg True (Just index)
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just ChooseOwnCardToSwitchFrontend

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text "Click on a card to switch"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                        let
                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text "You chose your card, now choose a card to switch with"
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        none
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing True Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand Nothing True (Just index)
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                        let
                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| "Remember! " ++ displayEndTimer counter
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width none

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
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                -- a fix avec le maybeindex
                                ++ displayAllOpponents maybeTamalouOwner Nothing True Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand Nothing True (Just index)
                                ]
                            ]

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

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                displayDiscardCards viewPort.width discardPile False Nothing

                            drewCardColumn : Element FrontendMsg
                            drewCardColumn =
                                el
                                    [ below <| el [ centerX, padding 12 ] <| text <| displayEndTimer timer
                                    , width fill
                                    , height fill
                                    ]
                                <|
                                    elEmplacement viewPort.width <|
                                        displayFCard Nothing FaceDown
                        in
                        column
                            ([ width <| px <| viewPort.width - 14
                             , height fill
                             , elPlacedByCenter drawPilePosition (displayDrawColumn drawPile False model.cardAnim)
                             , elPlacedByCenter drewCardPosition <| drewCardColumn
                             , elPlacedByCenter discardPilePosition <| discardPileColumn
                             ]
                                ++ displayAllOpponents maybeTamalouOwner Nothing False Nothing opponentsDisposition
                            )
                            [ column [ width fill, spacing 12, height fill ]
                                [ displayPlayerView viewPort.width model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                ]
                            ]

                    FGameEnded orderedPlayersAndRank ->
                        let
                            currentPlayerAndRank : Maybe ( FPlayer, Int )
                            currentPlayerAndRank =
                                List.Extra.find (\( player, _ ) -> Just player.sessionId == model.sessionId) orderedPlayersAndRank
                        in
                        column
                            [ width fill, spacing 12, padding 12 ]
                            [ el [ centerX ] <|
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
                            , column [ centerX, spacing 4, width <| px <| (viewPort.width * 80 // 100) ] <|
                                List.map (\player -> displayPlayerAndCards player) orderedPlayersAndRank
                            , el [ centerX ] <| actionButton { label = text "Play again!", onPress = Just (ReStartGameFrontend (currentPlayerAndRank |> Maybe.map Tuple.first)) }
                            ]

                    FGameAlreadyStartedWithoutYou ->
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ el [ centerY, centerX ] <| text "Sorry! The game already started without you, if you wanna play you can just go in a new url"
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
    el
        [ width fill
        , height fill
        , Border.rounded 8
        , Background.image "/emplacement.png"
        ]
    <|
        cardToDisplay


displayDiscardCards : Int -> DiscardPile -> Bool -> Maybe Card.Power -> Element FrontendMsg
displayDiscardCards widthOfScreen discardPile canDrawCard maybePowerCard =
    case ( discardPile, canDrawCard, maybePowerCard ) of
        ( [], _, _ ) ->
            elEmplacement widthOfScreen none

        ( head :: _, True, Just power ) ->
            el [ below <| Input.button [] { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend } ] <| elEmplacement widthOfScreen <| displayFCard Nothing (FaceUp head)

        ( head :: _, True, Nothing ) ->
            -- Warning, here, we put True because it's not possible that the queen power has been used the turn before with someone with a valid tamalou && 2 players.
            -- We could pass the nb players and maybeTamalouOwner but it's not necessary for now, we come up with a better solution later.
            case Card.toPower True head of
                Just _ ->
                    elEmplacement widthOfScreen <| displayFCard Nothing (FaceUp head)

                Nothing ->
                    elEmplacement widthOfScreen <| displayFCard (Just DrawFromDiscardPileFrontend) (FaceUp head)

        ( head :: _, False, _ ) ->
            elEmplacement widthOfScreen <| displayFCard Nothing (FaceUp head)


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
        [ spacing 12, centerX, Background.color isReadyColor, Border.rounded 8, paddingXY 4 4 ]
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
        [ spacing 12, centerX, Border.rounded 8, paddingXY 12 12, Background.color veryLightGrey, width fill, height <| px 64 ]
        [ text <| medal rank
        , el [ width <| px 250 ] <|
            text <|
                case player.name of
                    "" ->
                        "Anonymous"

                    playerName ->
                        playerName
        , el [] <| displayFCardsAtTheEnd player.tableHand
        , case player.score of
            Just score ->
                el [ alignRight ] <| text <| String.fromInt score

            Nothing ->
                none
        ]


veryLightGrey : Color
veryLightGrey =
    Element.rgb255 240 240 240


lightGrey : Color
lightGrey =
    Element.rgb255 220 220 220


displayPlayerView : Int -> Maybe SessionId -> Maybe String -> DeviceClass -> List FPlayer -> FTableHand -> Maybe (Int -> CardClickMsg) -> Bool -> Maybe Int -> Element FrontendMsg
displayPlayerView screenWidth _ _ _ _ tableHand maybeCardClick _ maybeIndex =
    row [ alignBottom, centerX ]
        [ displayOwnCards screenWidth tableHand maybeCardClick maybeIndex
        ]



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
--                 { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }
--             FaceDown ->
--                 { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


cardActionBorder : CardClickMsg -> List (Attribute FrontendMsg)
cardActionBorder cardClickMsg =
    let
        color : Color
        color =
            cardClickMsgToColor cardClickMsg
    in
    [ Border.rounded 8
    , Background.color color
    , bigShadow color
    , Events.onClick <| CardClickMsg cardClickMsg
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
    row [ spacing 4, centerX, height fill ] (List.indexedMap displayFCardAtTheEnd cards)


sizeOfCard : Int -> Int
sizeOfCard screenWidth =
    if screenWidth < 600 then
        5 * screenWidth // 36

    else if screenWidth < 800 then
        4 * screenWidth // 36

    else
        3 * screenWidth // 36


displayOwnCards : Int -> List FCard -> Maybe (Int -> CardClickMsg) -> Maybe Int -> Element FrontendMsg
displayOwnCards screenWidth cards maybeCardClickEvent maybeIndex =
    row [ spacing 12, height fill ] <|
        List.indexedMap (displayFCardSized (Just <| px <| sizeOfCard screenWidth) maybeCardClickEvent maybeIndex) cards


displayFCard : Maybe CardClickMsg -> FCard -> Element FrontendMsg
displayFCard maybeCardClickMsg frontendCard =
    el
        (case maybeCardClickMsg of
            Just cardClickMsg ->
                cardActionBorder cardClickMsg

            Nothing ->
                [ width fill ]
        )
    <|
        image [ width fill, height fill ] <|
            case frontendCard of
                FaceUp card ->
                    { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }

                FaceDown ->
                    { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardSized : Maybe Length -> Maybe (Int -> CardClickMsg) -> Maybe Int -> Int -> FCard -> Element FrontendMsg
displayFCardSized length maybeCardClickMsg maybeIndex index frontendCard =
    let
        attrs : List (Attribute FrontendMsg)
        attrs =
            Maybe.map (\cardClickMsg -> cardClickMsg index) maybeCardClickMsg |> Maybe.map cardActionBorder |> Maybe.withDefault []

        movedUp : List (Attribute FrontendMsg)
        movedUp =
            if maybeIndex == Just index then
                [ moveUp 12, bigShadow green ]

            else
                []
    in
    el [ width fill, height fill ] <|
        image (width (Maybe.withDefault fill length) :: attrs ++ movedUp) <|
            case frontendCard of
                FaceUp card ->
                    { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }

                FaceDown ->
                    { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardAtTheEnd : Int -> FCard -> Element FrontendMsg
displayFCardAtTheEnd =
    displayFCardSized (Just <| px 41) Nothing Nothing


actionBorder : Element.Color -> List (Attribute FrontendMsg)
actionBorder color =
    [ Border.rounded 8
    , Background.color color
    , paddingXY 4 4
    , minimalistShadow
    ]


minimalistShadow : Attr decorative FrontendMsg
minimalistShadow =
    Border.shadow
        { blur = 4
        , color = Element.rgba 0 0 0 0.2
        , offset = ( 2, 2 )
        , size = 1
        }


bigShadow : Element.Color -> Attr decorative FrontendMsg
bigShadow color =
    Border.shadow
        { blur = 8
        , color = color
        , offset = ( 0, 0 )
        , size = 6
        }


yellow : Color
yellow =
    Element.rgb255 238 221 136


blue : Color
blue =
    Element.rgb255 0 68 221


green : Color
green =
    Element.rgb255 35 187 34


red : Color
red =
    Element.rgb255 224 38 15


actionButton : { label : Element FrontendMsg, onPress : Maybe FrontendMsg } -> Element FrontendMsg
actionButton =
    Input.button (actionBorder yellow)


edges : { bottom : Int, left : Int, right : Int, top : Int }
edges =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


displayDrawColumn : List FCard -> Bool -> CardAnimation -> Element FrontendMsg
displayDrawColumn drawPile drawAllowed cardAnim =
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
            toFloat screenWidth / 11

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
            , positionedTableHand = List.indexedMap (\i c -> ( c, { x = leftSpace, y = 80 + 30 + toFloat i * (cardWidth + spaceBetweenEachCard), width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = pi / 2 } )) player.tableHand
            , namePosition = { x = leftSpace, y = 80, width_ = namePanelWidth, height_ = 30, rotation = 0 }
            }

        TopLeftPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, { x = leftSpace + namePanelWidth + spaceBetweenNameAndCards + toFloat i * (cardWidth + spaceBetweenEachCard), y = 0, width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = pi } )) player.tableHand
            , namePosition = { x = leftSpace, y = 8, width_ = namePanelWidth, height_ = 30, rotation = 0 }
            }

        TopRightPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, { x = toFloat screenWidth - toFloat i * (cardWidth + spaceBetweenEachCard), y = 0, width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = pi } )) player.tableHand
            , namePosition = { x = toFloat screenWidth - panelWidth - (namePanelWidth / 2), y = 0, width_ = namePanelWidth, height_ = 30, rotation = 0 }
            }

        RightPlayer ->
            { player = player
            , positionedTableHand = List.indexedMap (\i c -> ( c, { x = 0, y = toFloat i * (cardWidth + spaceBetweenEachCard), width_ = cardWidth, height_ = cardWidth * heightCardRatio, rotation = 3 * pi / 2 } )) player.tableHand
            , namePosition = { x = toFloat screenWidth - (namePanelWidth / 2), y = 50, width_ = namePanelWidth, height_ = 30, rotation = 0 }
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
            displayOpponentName positionedPlayer.namePosition isPlayerTurn positionedPlayer.player.name :: List.indexedMap (\i ( c, position ) -> elPlaced position <| displayFCardSized Nothing switchingCardsMsg maybeCardIndex i c) positionedPlayer.positionedTableHand

        Nothing ->
            []


displayOpponentName : Position -> Bool -> String -> Attribute FrontendMsg
displayOpponentName pos isPlayerTurn name =
    elPlaced pos
        (el
            ([ width fill, height fill, padding 4, Border.rounded 8, Border.width 1, Font.size 11 ]
                ++ (if isPlayerTurn then
                        [ Background.color yellow, Border.color yellow ]

                    else
                        [ Border.color blue ]
                   )
            )
         <|
            paragraph [ spacing 4, Font.center, centerY ] <|
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


cardMoveAndFlip : Position -> Position -> CardAnimation -> Element FrontendMsg
cardMoveAndFlip oldPosition newPosition cardAnimation =
    case cardAnimation of
        CardFlipped card ->
            none

        -- displayFCard Nothing (FaceUp card)
        CardNotFlipped ->
            none

        -- displayFCard (Just DrawCardFromDeckFrontend) FaceDown
        CardFlipping fCard ->
            Anim.el (cardFlip oldPosition newPosition) [ htmlAttribute <| HA.style "z-index" "10" ] <| displayFCard Nothing fCard



-- type alias Position =
--     { x : Float
--     , y : Float
--     , width_ : Float
--     , height_ : Float
--     , rotation : Float
--     }


cardFlip : Position -> Position -> SAnimation.Animation
cardFlip oldPosition newPosition =
    SAnimation.steps
        { startAt = [ SP.scaleXY 1 1, SP.x oldPosition.x, SP.y oldPosition.y, SP.rotate oldPosition.rotation ]
        , options = [ easeInOutQuad ]
        }
        [ SAnimation.step 1000 [ SP.scaleXY 0 1, SP.x newPosition.x, SP.y newPosition.y, SP.rotate newPosition.rotation ]
        , SAnimation.step 1000 [ SP.scaleXY 1 1, SP.x newPosition.x, SP.y newPosition.y, SP.rotate newPosition.rotation ]
        ]



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


elPlacedByCenter : Position -> Element FrontendMsg -> Attribute FrontendMsg
elPlacedByCenter { x, y, width_, height_, rotation } =
    inFront << el [ moveDown <| (y - (height_ / 2)), moveRight <| (x - (width_ / 2)), rotate rotation, width <| px <| round width_, height <| px <| round height_ ]


elPlaced : Position -> Element FrontendMsg -> Attribute FrontendMsg
elPlaced { x, y, width_, height_, rotation } =
    inFront << el [ moveDown <| y, moveRight <| x, rotate rotation, width <| px <| round width_, height <| px <| round height_ ]


calculateDrawPilePosition : Int -> Int -> Position
calculateDrawPilePosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.35
    , y = toFloat screenHeight * 0.4
    , width_ = cardWidthInMiddle screenWidth
    , height_ = cardWidthInMiddle screenWidth * heightCardRatio
    , rotation = 0
    }


calculateDrewCardPosition : Int -> Int -> Position
calculateDrewCardPosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.5
    , y = toFloat screenHeight * 0.4
    , width_ = cardWidthInMiddle screenWidth
    , height_ = cardWidthInMiddle screenWidth * heightCardRatio
    , rotation = 0
    }


calculateDiscardPilePosition : Int -> Int -> Position
calculateDiscardPilePosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.65
    , y = toFloat screenHeight * 0.4
    , width_ = cardWidthInMiddle screenWidth
    , height_ = cardWidthInMiddle screenWidth * heightCardRatio
    , rotation = 0
    }


calculateTamalouButtonPosition : Int -> Int -> Position
calculateTamalouButtonPosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.5
    , y = toFloat screenHeight * 0.65
    , width_ = 65
    , height_ = 20
    , rotation = 0
    }


calculatePlayAgainOrPassPosition : Int -> Int -> Position
calculatePlayAgainOrPassPosition screenWidth screenHeight =
    { x = toFloat screenWidth * 0.5
    , y = toFloat screenHeight * 0.65
    , width_ = 108
    , height_ = 20
    , rotation = 0
    }


calculateGameDisposition : { height : Int, width : Int } -> List FPlayer -> GameDisposition
calculateGameDisposition viewPort opponents =
    Calculated
        { drawPilePosition = calculateDrawPilePosition viewPort.width viewPort.height
        , drewCardPosition = calculateDrewCardPosition viewPort.width viewPort.height
        , discardPilePosition = calculateDiscardPilePosition viewPort.width viewPort.height
        , tamalouButtonPosition = calculateTamalouButtonPosition viewPort.width viewPort.height
        , playAgainOrPassPosition = calculatePlayAgainOrPassPosition viewPort.width viewPort.height
        , opponentsDisposition = toOpponentsDisposition viewPort.width opponents
        }


displayAllOpponents : Maybe TamalouOwner -> Maybe SessionId -> Bool -> Maybe Int -> OpponentsDisposition -> List (Attribute FrontendMsg)
displayAllOpponents maybeTamalouOwner maybeSessionId isSwitchingCard maybeCardIndex opponentsDisposition =
    [ displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.topLeftPlayer maybeCardIndex
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.topRightPlayer maybeCardIndex
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.leftPlayer maybeCardIndex
    , displayOpponent maybeTamalouOwner maybeSessionId isSwitchingCard opponentsDisposition.rightPlayer maybeCardIndex
    ]
        |> List.concat
