module Frontend exposing (CardClickEvent(..), OpponentDisposition(..), OpponentsDisposition, app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Element exposing (Attr, Attribute, Color, Device, DeviceClass(..), Element, Length, Orientation(..), alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, classifyDevice, column, el, fill, fillPortion, height, html, htmlAttribute, image, moveUp, none, padding, paddingEach, paddingXY, paragraph, px, rotate, row, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HA
import Lamdera exposing (SessionId)
import List.Extra
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Anim
import Simple.Animation.Property as Anim
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Task
import Types exposing (ActionFromGameToBackend(..), Counter(..), DiscardPile, FGame(..), FGameInProgressStatus(..), FPlayer, FPlayerToPlayStatus(..), FTableHand, FrontendModel, FrontendMsg(..), LookACardStatus(..), Switch2CardsStatus(..), TamalouOwner, ToBackend(..), ToFrontend(..))
import Url


grey : Color
grey =
    Element.rgb255 0 0 0


phoneRotateAnimation : Animation
phoneRotateAnimation =
    Animation.steps
        { options = [ Animation.loop ]
        , startAt = [ Anim.rotate 0, customTransformOrigin "center" ]
        }
        [ Animation.step 500 [ Anim.rotate 90, customTransformOrigin "center" ]
        , Animation.wait 300
        , Animation.step 200 [ Anim.rotate 0, customTransformOrigin "center" ]
        , Animation.wait 300
        ]


customTransformOrigin : String -> Anim.Property
customTransformOrigin origin =
    Anim.property "transform-origin" origin


minimalistPhoneWithHint : Svg FrontendMsg
minimalistPhoneWithHint =
    Svg.svg [ SvgA.viewBox "0 0 100 100" ]
        [ Anim.svg { class = SvgA.class } Svg.g phoneRotateAnimation [] [ phoneSvg ]
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
      , screenHeight = 0
      , screenWidth = 0
      , ready = False
      , maybeName = Nothing
      , chatInput = ""
      , chat = []
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
            ( { model | device = classifyDevice viewPort, screenHeight = viewPort.height, screenWidth = viewPort.width }
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

        DrawCardFromDeckFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawCardFromDrawPileToBackend )

        TamalouFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath TamalouToBackend )

        DiscardCardFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DiscardCardInHandToBackend )

        DrawFromDiscardPileFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawFromDiscardPileToBackend )

        PowerIsUsedFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath PowerIsUsedToBackend )

        PowerPassFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath PowerIsNotUsedToBackend )

        ReplaceCardInFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ReplaceCardInTableHandToBackend cardIndex) )

        DoubleCardFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (DoubleCardInTableHandToBackend cardIndex) )

        LookAtCardFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (LookAtCardInTableHandToBackend cardIndex) )

        ChooseOwnCardToSwitchFrontend cardIndex ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChooseOwnCardToSwitchToBackend cardIndex) )

        ChooseOpponentCardToSwitchFrontend ( sessionId, cardIndex ) ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChooseOpponentCardToSwitchToBackend ( sessionId, cardIndex )) )

        ChangeChatInputFrontend newChatInput ->
            ( { model | chatInput = newChatInput }, Cmd.none )

        SendMessageFrontend ->
            ( { model | chatInput = "", chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, model.chatInput ) ] }
            , Cmd.batch
                [ Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (SendMessageToBackend model.chatInput)
                , scrollToBottom "chatty"
                ]
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
            orderedPlayers

        FGameAlreadyStartedWithoutYou ->
            []


getMyName : Maybe SessionId -> FGame -> Maybe String
getMyName maybeSessionId fGame =
    fPlayersFromFGame fGame
        |> List.Extra.find (\player -> maybeSessionId == Just player.sessionId)
        |> Maybe.map .name


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        safeAreaStyle : Html.Attribute msg
        safeAreaStyle =
            HA.style "padding"
                "env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left)"
    in
    { title = "Tamalou!"
    , body =
        [ Element.layout
            [ width <| px model.screenWidth
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
        [ centerX, centerY, spacing 20, scrollbars ]
        [ if model.admin then
            displayAdmin model

          else
            displayGame model
        ]


type CardClickEvent
    = CardClickReplacement
    | CardClickDouble
    | LookThisCard
    | SwitchCard


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
                        , displayChat fModel.screenWidth fModel.screenHeight fModel.chatInput fModel.chat
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


displayGame : FrontendModel -> Element FrontendMsg
displayGame ({ screenWidth } as model) =
    case ( model.device.class, model.device.orientation ) of
        ( Phone, Portrait ) ->
            column [ Font.center, width fill, centerY ]
                [ el [ width fill, centerX ] <| text "Rotate your phone 🚀"
                , el [ centerX, height <| px 150, width <| px 150, moveUp 24 ] <| html minimalistPhoneWithHint
                ]

        _ ->
            el [ width fill, height fill, padding 12 ] <|
                case model.fGame of
                    FWaitingForPlayers players ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ displayGameLobby model players ]

                    FGameInProgress _ _ _ _ players (FStartTimerRunning Five) ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ el [ centerX, centerY ] <| displayStartTimer Five
                            , displayPlayerView model.sessionId model.maybeName model.device.class players [ FaceDown, FaceDown, FaceDown, FaceDown ] Nothing False
                            ]

                    FGameInProgress _ hand _ _ players (FStartTimerRunning timer) ->
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ el [ centerX, centerY ] <| displayStartTimer timer
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand Nothing False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FWaitingPlayerAction _)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerHasDraw _)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <| displayFCard Phone FaceDown
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8, width fill ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerHasDiscard _)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerLookACard _)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ centerX ] <| text <| fPlayer.name ++ " is choosing a card to switch"
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OwnCardChosen _))) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] (displayDiscardCards screenWidth discardPile False Nothing)

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ centerX ] <| text <| fPlayer.name ++ " chose his card, now choose a card to switch with"
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FWaitingPlayerAction maybePowerCard)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile True maybePowerCard

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile True
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , case maybeTamalouOwner of
                                Just _ ->
                                    none

                                Nothing ->
                                    el [ centerX, paddingEach { edges | bottom = 24, top = 12 }, Font.color blue, Font.italic ] <|
                                        Input.button (actionBorder yellow)
                                            { label = text "\"Tamalou!\"", onPress = Just TamalouFrontend }
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
                        let
                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| el (cardActionBorder yellow DiscardCardFrontend) <| displayFCard Phone fCard ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand (Just CardClickReplacement) True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDiscard power)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False (Just power)

                            displayUsePowerOrPass : Element FrontendMsg
                            displayUsePowerOrPass =
                                row [ centerX, spacing 8 ]
                                    [ actionButton { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend }
                                    , actionButton { label = text "Pass", onPress = Just PowerPassFrontend }
                                    ]

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , displayUsePowerOrPass
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard ChooseCardToLook)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                Just LookThisCard

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ centerX ] <| text "Click on a card to look at it"
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard (LookingACard counter))) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ centerX ] <| displayEndTimer counter
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                Just SwitchCard

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ centerX ] <| text "Click on a card to switch"
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OwnCardChosen _))) ->
                        let
                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing True <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing True <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing True <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing True <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ centerX ] <| text <| "You chose your card, now choose a card to switch with"
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand Nothing True
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FEndTimerRunning timer) ->
                        let
                            cardClickEvent : Maybe CardClickEvent
                            cardClickEvent =
                                if List.isEmpty discardPile then
                                    Nothing

                                else if Maybe.map .sessionId maybeTamalouOwner == model.sessionId then
                                    Nothing

                                else
                                    Just CardClickDouble

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <| displayFCard Phone FaceDown
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8, width fill ]
                                    (displayDiscardCards screenWidth discardPile False Nothing)

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ row [ height <| px 64, width fill ] [ displayOpponent maybeTamalouOwner Nothing False <| TopLeftPlayer opponentsDisposition.topLeftPlayer, displayOpponent maybeTamalouOwner Nothing False <| TopRightPlayer opponentsDisposition.topRightPlayer ]
                            , row
                                [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False <| LeftPlayer opponentsDisposition.leftPlayer
                                , row [ spacing 16, centerX, centerY ]
                                    [ displayDrawColumn screenWidth drawPile False
                                    , currentCardColumn
                                    , discardPileColumn
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False <| RightPlayer opponentsDisposition.rightPlayer
                                ]
                            , el [ Font.center, width fill ] <| displayEndTimer timer
                            , displayPlayerView model.sessionId model.maybeName model.device.class players hand cardClickEvent False
                            ]

                    FGameEnded orderedPlayers ->
                        let
                            currentPlayer : Maybe { name : String, tableHand : List FCard, clientId : Lamdera.ClientId, sessionId : SessionId, ready : Bool, score : Maybe Int }
                            currentPlayer =
                                List.Extra.find (\player -> Just player.sessionId == model.sessionId) orderedPlayers

                            rank : Maybe Int
                            rank =
                                orderedPlayers
                                    |> List.Extra.findIndex (\player -> Just player.sessionId == model.sessionId)
                        in
                        column
                            [ width fill, height fill, spacing 20, scrollbars ]
                            [ el [ centerX ] <|
                                text <|
                                    case rank of
                                        Just 0 ->
                                            "You win!🥇"

                                        Just 1 ->
                                            "Maybe next time!🥈"

                                        Just 2 ->
                                            "Luck is not on your side!🥉"

                                        Just 3 ->
                                            "You lost! Here's a cookie 🍪"

                                        Just _ ->
                                            "You lost!🤷\u{200D}♂️"

                                        Nothing ->
                                            "Game ended!"
                            , column [ centerX, spacing 4, width <| px <| (screenWidth * 80 // 100) ] <|
                                List.indexedMap (\i player -> displayPlayerAndCards i player) orderedPlayers
                            , el [ centerX ] <| actionButton { label = text "Play again!", onPress = Just (ReStartGameFrontend currentPlayer) }
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

        ( head :: _, True, Just power ) ->
            [ elEmplacement widthOfScreen <| displayFCard Phone (FaceUp head)
            , Input.button [] { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend }
            ]

        ( head :: _, True, Nothing ) ->
            -- Warning, here, we put True because it's not possible that the queen power has been used the turn before with someone with a valid tamalou && 2 players.
            -- We could pass the nb players and maybeTamalouOwner but it's not necessary for now, we come up with a better solution later.
            case Card.toPower True head of
                Just _ ->
                    [ elEmplacement widthOfScreen <| displayFCard Phone (FaceUp head) ]

                Nothing ->
                    [ elEmplacement widthOfScreen <| el (cardActionBorder yellow DrawFromDiscardPileFrontend) <| displayFCard Phone (FaceUp head) ]

        ( head :: _, False, _ ) ->
            [ elEmplacement widthOfScreen <| displayFCard Phone (FaceUp head) ]


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


displayPlayerAndCards : Int -> FPlayer -> Element FrontendMsg
displayPlayerAndCards rank player =
    row
        [ spacing 12, centerX, Border.rounded 8, paddingXY 12 12, Background.color veryLightGrey, width fill, height <| px 100 ]
        [ text <| medal rank
        , text <|
            case player.name of
                "" ->
                    "Anonymous"

                playerName ->
                    playerName
        , el [ width fill ] <| displayFCardsAtTheEnd player.tableHand
        , case player.score of
            Just score ->
                text <| String.fromInt score

            Nothing ->
                none
        ]


veryLightGrey : Color
veryLightGrey =
    Element.rgb255 240 240 240


lightGrey : Color
lightGrey =
    Element.rgb255 220 220 220


displayPlayerView : Maybe SessionId -> Maybe String -> DeviceClass -> List FPlayer -> FTableHand -> Maybe CardClickEvent -> Bool -> Element FrontendMsg
displayPlayerView _ maybeName deviceClass _ tableHand maybeCardClick isPlayerTurn =
    column [ width fill, alignBottom ]
        [ displayOwnName maybeName isPlayerTurn
        , displayFCards deviceClass tableHand maybeCardClick
        ]


displayFCards : DeviceClass -> List FCard -> Maybe CardClickEvent -> Element FrontendMsg
displayFCards deviceClass cards maybeCardClickEvent =
    row [ spacing 4, centerX, width fill, paddingXY 128 0, height fill ] (List.indexedMap (onClickCard maybeCardClickEvent (displayFCard deviceClass)) cards)


displayFCardsAtTheEnd : List FCard -> Element FrontendMsg
displayFCardsAtTheEnd cards =
    row [ spacing 4, centerX, height fill ] (List.map (displayFCardSized <| px 96) cards)


onClickCard : Maybe CardClickEvent -> (FCard -> Element FrontendMsg) -> Int -> FCard -> Element FrontendMsg
onClickCard maybeCardClickEvent tag index card =
    case maybeCardClickEvent of
        Just CardClickReplacement ->
            el (width fill :: cardActionBorder yellow (ReplaceCardInFrontend index)) (tag card)

        Just CardClickDouble ->
            el (width fill :: cardActionBorder blue (DoubleCardFrontend index)) (tag card)

        Just LookThisCard ->
            el (width fill :: cardActionBorder yellow (LookAtCardFrontend index)) (tag card)

        Just SwitchCard ->
            el (width fill :: cardActionBorder green (ChooseOwnCardToSwitchFrontend index)) (tag card)

        Nothing ->
            tag card


displayFCard : DeviceClass -> FCard -> Element FrontendMsg
displayFCard _ frontendCard =
    image [ width fill, height fill ] <|
        case frontendCard of
            FaceUp card ->
                { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }

            FaceDown ->
                { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardSized : Length -> FCard -> Element FrontendMsg
displayFCardSized length frontendCard =
    image [ height length, centerX, centerY ] <|
        case frontendCard of
            FaceUp card ->
                { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }

            FaceDown ->
                { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardSizedVertically : Length -> FCard -> Element FrontendMsg
displayFCardSizedVertically length frontendCard =
    image [ height length, centerX, centerY, rotate (pi / 2) ] <|
        case frontendCard of
            FaceUp card ->
                { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }

            FaceDown ->
                { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


actionBorder : Element.Color -> List (Attribute FrontendMsg)
actionBorder color =
    [ Border.rounded 8
    , Background.color color
    , paddingXY 4 4
    , minimalistShadow
    ]


cardActionBorder : Element.Color -> FrontendMsg -> List (Attribute FrontendMsg)
cardActionBorder color frontendMsg =
    [ Border.rounded 8
    , Background.color color
    , bigShadow color
    , Events.onClick frontendMsg
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


displayDrawColumn : Int -> List FCard -> Bool -> Element FrontendMsg
displayDrawColumn screenWidth drawPile drawAllowed =
    elEmplacement screenWidth <|
        if drawAllowed then
            el (cardActionBorder yellow DrawCardFromDeckFrontend) <| displayFCard Phone FaceDown

        else if List.isEmpty drawPile then
            none

        else
            displayFCard Phone FaceDown


displayOpponentRow : FPlayer -> Bool -> Bool -> Bool -> Element FrontendMsg
displayOpponentRow player isPlayerTurn isSwitchingCard isTamalouOwner =
    let
        attrsOwnTurn : List (Attribute FrontendMsg)
        attrsOwnTurn =
            if isPlayerTurn then
                [ Background.color yellow, Border.color yellow ]

            else
                [ Border.color blue ]

        switchingCardsAttrs : Int -> List (Attribute FrontendMsg)
        switchingCardsAttrs index =
            if isSwitchingCard && not isTamalouOwner then
                cardActionBorder green <| ChooseOpponentCardToSwitchFrontend ( player.sessionId, index )

            else
                []
    in
    row
        [ spacing 8 ]
        [ el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4 ] ++ attrsOwnTurn) <| text player.name
        , row [ spacing 4, centerX, height fill ] <|
            List.indexedMap
                (\index card ->
                    el
                        ([ width fill
                         , height fill
                         ]
                            ++ switchingCardsAttrs index
                        )
                    <|
                        displayFCardSized (px 60) card
                )
                (player.tableHand |> List.reverse)
        ]


displayOpponentColumn : FPlayer -> Bool -> Bool -> Bool -> Element FrontendMsg
displayOpponentColumn player isPlayerTurn isSwitchingCard isTamalouOwner =
    let
        attrsOwnTurn : List (Attribute FrontendMsg)
        attrsOwnTurn =
            if isPlayerTurn then
                [ Background.color yellow, Border.color yellow ]

            else
                [ Border.color blue ]

        switchingCardsAttrs : Int -> List (Attribute FrontendMsg)
        switchingCardsAttrs index =
            if isSwitchingCard && not isTamalouOwner then
                cardActionBorder green <| ChooseOpponentCardToSwitchFrontend ( player.sessionId, index )

            else
                []
    in
    column
        [ spacing 8, alignTop ]
        [ el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4 ] ++ attrsOwnTurn) <| text player.name
        , column [ spacing -16, centerX, width fill ] <|
            List.indexedMap
                (\index card ->
                    el
                        ([ height fill
                         , width fill
                         ]
                            ++ switchingCardsAttrs index
                        )
                    <|
                        displayFCardSizedVertically (px 60) card
                )
                (player.tableHand |> List.reverse)
        ]


displayOwnName : Maybe String -> Bool -> Element FrontendMsg
displayOwnName maybeName isPlayerTurn =
    let
        attrs : List (Attr decorative FrontendMsg)
        attrs =
            if isPlayerTurn then
                [ Background.color yellow, Border.color yellow ]

            else
                [ Border.color blue ]
    in
    el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4 ] ++ attrs) <|
        case maybeName of
            Just name ->
                text name

            Nothing ->
                text "Anonymous"


type OpponentDisposition
    = LeftPlayer (Maybe FPlayer)
    | TopLeftPlayer (Maybe FPlayer)
    | TopRightPlayer (Maybe FPlayer)
    | RightPlayer (Maybe FPlayer)


type alias OpponentsDisposition =
    { leftPlayer : Maybe FPlayer
    , topLeftPlayer : Maybe FPlayer
    , topRightPlayer : Maybe FPlayer
    , rightPlayer : Maybe FPlayer
    }


toOpponentsDisposition : List FPlayer -> OpponentsDisposition
toOpponentsDisposition players =
    case players of
        [] ->
            { leftPlayer = Nothing, topLeftPlayer = Nothing, topRightPlayer = Nothing, rightPlayer = Nothing }

        [ oneOpponent ] ->
            { leftPlayer = Nothing, topLeftPlayer = Just oneOpponent, topRightPlayer = Nothing, rightPlayer = Nothing }

        [ firstOpponent, secondOpponent ] ->
            { leftPlayer = Nothing, topLeftPlayer = Just firstOpponent, topRightPlayer = Just secondOpponent, rightPlayer = Nothing }

        [ firstOpponent, secondOpponent, thirdOpponent ] ->
            { leftPlayer = Just firstOpponent, topLeftPlayer = Just secondOpponent, topRightPlayer = Just thirdOpponent, rightPlayer = Nothing }

        [ firstOpponent, secondOpponent, thirdOpponent, fourthOpponent ] ->
            { leftPlayer = Just firstOpponent, topLeftPlayer = Just secondOpponent, topRightPlayer = Just thirdOpponent, rightPlayer = Just fourthOpponent }

        _ ->
            { leftPlayer = Nothing, topLeftPlayer = Nothing, topRightPlayer = Nothing, rightPlayer = Nothing }


displayOpponent : Maybe TamalouOwner -> Maybe SessionId -> Bool -> OpponentDisposition -> Element FrontendMsg
displayOpponent maybeTamalouOwner maybeSessionId switchingCard opponentDisposition =
    let
        isPlayerTurn : SessionId -> Bool
        isPlayerTurn playerId =
            maybeSessionId == Just playerId

        isTamalouOwner : SessionId -> Bool
        isTamalouOwner sessionId =
            case maybeTamalouOwner of
                Just tamalouOwner ->
                    tamalouOwner.sessionId == sessionId

                Nothing ->
                    False
    in
    case opponentDisposition of
        LeftPlayer (Just player) ->
            el [ alignLeft ] <| displayOpponentColumn player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId)

        TopLeftPlayer (Just player) ->
            el [ alignLeft ] <| displayOpponentRow player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId)

        TopRightPlayer (Just player) ->
            el [ alignRight ] <| displayOpponentRow player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId)

        RightPlayer (Just player) ->
            el [ alignRight ] <| displayOpponentColumn player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId)

        _ ->
            none
