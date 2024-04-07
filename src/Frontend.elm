module Frontend exposing (OpponentDisposition(..), OpponentsDisposition, app)

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
import Html.Attributes as HA
import Lamdera exposing (SessionId)
import List.Extra
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Anim
import Simple.Animation.Property as Anim
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Task
import Types exposing (ActionFromGameToBackend(..), CardClickMsg(..), Counter(..), DiscardPile, FGame(..), FGameInProgressStatus(..), FPlayer, FPlayerToPlayStatus(..), FTableHand, FrontendModel, FrontendMsg(..), LookACardStatus(..), Switch2CardsStatus(..), TamalouOwner, ToBackend(..), ToFrontend(..))
import Url


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
                    ( model, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawCardFromDrawPileToBackend )

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
    { title = "Tamalou!"
    , body =
        [ Element.layout
            [ width <| px model.screenWidth
            , height <| px model.screenHeight
            , Background.image "/background.png"
            , Font.size 12
            ]
          <|
            displayModel model
        ]
    }


displayModel : FrontendModel -> Element FrontendMsg
displayModel model =
    Element.column
        [ width fill, height fill ]
        [ if model.admin then
            displayAdmin model

          else
            displayGame model
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
                            , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players [ FaceDown, FaceDown, FaceDown, FaceDown ] Nothing False Nothing
                            ]

                    FGameInProgress _ hand _ _ players (FStartTimerRunning timer) ->
                        column
                            [ width fill, height fill, spacing 20 ]
                            [ el [ centerX, centerY ] <| displayStartTimer timer
                            , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand Nothing False Nothing
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| "It's " ++ fPlayer.name ++ "'s turn"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) Nothing
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

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <| displayFCard Nothing FaceDown
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| fPlayer.name ++ " just drew a card"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) Nothing
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text (fPlayer.name ++ " can choose to use a power or not")
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) Nothing
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

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                case lookACardStatus of
                                    LookingACard index _ ->
                                        if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                            Just index

                                        else
                                            Nothing

                                    ChooseCardToLook ->
                                        Nothing
                        in
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) (maybeIndex opponentsDisposition.topLeftPlayer)
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) (maybeIndex opponentsDisposition.topRightPlayer)
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) (maybeIndex opponentsDisposition.leftPlayer)
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , case lookACardStatus of
                                        ChooseCardToLook ->
                                            el [ centerX ] <| text <| fPlayer.name ++ " is choosing a card to look at"

                                        LookingACard _ counter ->
                                            el [ centerX ] <| text <| fPlayer.name ++ " is looking at a card: " ++ displayEndTimer counter
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) (maybeIndex opponentsDisposition.rightPlayer)
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just DoubleCardFrontend

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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| fPlayer.name ++ " is choosing a card to switch"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OwnCardChosen index))) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just DoubleCardFrontend

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

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                    Just index

                                else
                                    Nothing
                        in
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) (maybeIndex opponentsDisposition.topLeftPlayer)
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) (maybeIndex opponentsDisposition.topRightPlayer)
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) (maybeIndex opponentsDisposition.leftPlayer)
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| fPlayer.name ++ " is now choosing an opponent card to switch with"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) (maybeIndex opponentsDisposition.rightPlayer)
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FPlayerToPlay fPlayer (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
                        let
                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| none
                                    , none
                                    ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] (displayDiscardCards screenWidth discardPile False Nothing)

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                if (maybePlayer |> Maybe.map .sessionId) == Just fPlayer.sessionId then
                                    Just index

                                else if (maybePlayer |> Maybe.map .sessionId) == Just opponentCard.sessionId then
                                    Just opponentCard.index

                                else
                                    Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players

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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopLeftPlayer opponentsDisposition.topLeftPlayer) (maybeIndex opponentsDisposition.topLeftPlayer)
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (TopRightPlayer opponentsDisposition.topRightPlayer) (maybeIndex opponentsDisposition.topRightPlayer)
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (LeftPlayer opponentsDisposition.leftPlayer) (maybeIndex opponentsDisposition.leftPlayer)
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| fPlayer.name ++ " changed a card with " ++ (opponent |> Maybe.map .name |> Maybe.withDefault "Anonymous") ++ "'s card: " ++ displayEndTimer counter
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand Nothing False maybeOwnIndex
                                    ]
                                , displayOpponent maybeTamalouOwner (Just fPlayer.sessionId) False (RightPlayer opponentsDisposition.rightPlayer) (maybeIndex opponentsDisposition.rightPlayer)
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

                            tamalouButton : Element FrontendMsg
                            tamalouButton =
                                case maybeTamalouOwner of
                                    Just _ ->
                                        el [ centerX, paddingEach { edges | bottom = 16, top = 12 } ] none

                                    Nothing ->
                                        el [ centerX, paddingEach { edges | bottom = 16, top = 12 }, Font.color blue, Font.italic ] <|
                                            Input.button (actionBorder yellow)
                                                { label = text "\"Tamalou!\"", onPress = Just TamalouFrontend }
                        in
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile True
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , tamalouButton
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
                        let
                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8 ]
                                    [ elEmplacement screenWidth <| displayFCard (Just DiscardCardFrontend) fCard ]

                            discardPileColumn : Element FrontendMsg
                            discardPileColumn =
                                column [ spacing 8 ] <| displayDiscardCards screenWidth discardPile False Nothing

                            opponentsDisposition : OpponentsDisposition
                            opponentsDisposition =
                                toOpponentsDisposition players
                        in
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text "You just drew a card"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand (Just CardClickReplacement) True Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , displayUsePowerOrPass
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerLookACard ChooseCardToLook)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just LookAtCardFrontend

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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text "Click on a card to look at it"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| displayEndTimer counter
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg True (Just index)
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards ChooseOwnCardToSwitch)) ->
                        let
                            cardClickMsg : Maybe (Int -> CardClickMsg)
                            cardClickMsg =
                                Just ChooseOwnCardToSwitchFrontend

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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text "Click on a card to switch"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg True Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OwnCardChosen index))) ->
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing True (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing True (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing True (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| "You chose your card, now choose a card to switch with"
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand Nothing True (Just index)
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing True (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
                            ]

                    FGameInProgress maybeTamalouOwner hand drawPile discardPile players (FYourTurn (FPlayerSwitch2Cards (OpponentCardChosen index opponentCard counter))) ->
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

                            maybeIndex : Maybe FPlayer -> Maybe Int
                            maybeIndex maybePlayer =
                                if (maybePlayer |> Maybe.map .sessionId) == Just opponentCard.sessionId then
                                    Just opponentCard.index

                                else
                                    Nothing
                        in
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing True (TopLeftPlayer opponentsDisposition.topLeftPlayer) (maybeIndex opponentsDisposition.topLeftPlayer)
                                , displayOpponent maybeTamalouOwner Nothing True (TopRightPlayer opponentsDisposition.topRightPlayer) (maybeIndex opponentsDisposition.topRightPlayer)
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing True (LeftPlayer opponentsDisposition.leftPlayer) (maybeIndex opponentsDisposition.leftPlayer)
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ centerX ] <| text <| "Remember! " ++ displayEndTimer counter
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand Nothing True (Just index)
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing True (RightPlayer opponentsDisposition.rightPlayer) (maybeIndex opponentsDisposition.rightPlayer)
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

                            currentCardColumn : Element FrontendMsg
                            currentCardColumn =
                                column [ spacing 8, width fill ]
                                    [ elEmplacement screenWidth <| displayFCard Nothing FaceDown
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
                        column [ width <| px <| screenWidth - 14, height fill ]
                            [ row [ width fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (TopLeftPlayer opponentsDisposition.topLeftPlayer) Nothing
                                , displayOpponent maybeTamalouOwner Nothing False (TopRightPlayer opponentsDisposition.topRightPlayer) Nothing
                                ]
                            , row [ width fill, height fill ]
                                [ displayOpponent maybeTamalouOwner Nothing False (LeftPlayer opponentsDisposition.leftPlayer) Nothing
                                , column [ width fill, spacing 12, height fill ]
                                    [ row [ spacing 16, centerX, paddingEach { edges | top = 24 } ]
                                        [ displayDrawColumn screenWidth drawPile False
                                        , currentCardColumn
                                        , discardPileColumn
                                        ]
                                    , el [ Font.center, width fill ] <| text <| displayEndTimer timer
                                    , displayPlayerView model.screenWidth model.sessionId model.maybeName model.device.class players hand cardClickMsg False Nothing
                                    ]
                                , displayOpponent maybeTamalouOwner Nothing False (RightPlayer opponentsDisposition.rightPlayer) Nothing
                                ]
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
                            [ width fill, height fill, spacing 20 ]
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
                            [ width fill, height fill, spacing 20 ]
                            [ el [ centerY, centerX ] <| text "Sorry! The game already started without you, if you wanna play you can just go in a new url"
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
        [ width <| px <| widthOfScreen // 10
        , height <| px <| widthOfScreen * 15 // 100
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
            [ elEmplacement widthOfScreen <| displayFCard Nothing (FaceUp head)
            , Input.button [] { label = text <| Card.powerToString power, onPress = Just PowerIsUsedFrontend }
            ]

        ( head :: _, True, Nothing ) ->
            -- Warning, here, we put True because it's not possible that the queen power has been used the turn before with someone with a valid tamalou && 2 players.
            -- We could pass the nb players and maybeTamalouOwner but it's not necessary for now, we come up with a better solution later.
            case Card.toPower True head of
                Just _ ->
                    [ elEmplacement widthOfScreen <| displayFCard Nothing (FaceUp head) ]

                Nothing ->
                    [ elEmplacement widthOfScreen <| displayFCard (Just DrawFromDiscardPileFrontend) (FaceUp head) ]

        ( head :: _, False, _ ) ->
            [ elEmplacement widthOfScreen <| displayFCard Nothing (FaceUp head) ]


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


displayPlayerView : Int -> Maybe SessionId -> Maybe String -> DeviceClass -> List FPlayer -> FTableHand -> Maybe (Int -> CardClickMsg) -> Bool -> Maybe Int -> Element FrontendMsg
displayPlayerView screenWidth _ maybeName _ _ tableHand maybeCardClick isPlayerTurn maybeIndex =
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
        List.indexedMap (displayFCardSized (px <| sizeOfCard screenWidth) maybeCardClickEvent maybeIndex) cards


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


displayFCardSized : Length -> Maybe (Int -> CardClickMsg) -> Maybe Int -> Int -> FCard -> Element FrontendMsg
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
    image (width length :: attrs ++ movedUp) <|
        case frontendCard of
            FaceUp card ->
                { description = Card.toString card, src = "/cardImages/" ++ Card.toString card ++ ".png" }

            FaceDown ->
                { description = "back", src = "/cardImages/BackCovers/Pomegranate.png" }


displayFCardAtTheEnd : Int -> FCard -> Element FrontendMsg
displayFCardAtTheEnd =
    displayFCardSized (px 96) Nothing Nothing


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


displayDrawColumn : Int -> List FCard -> Bool -> Element FrontendMsg
displayDrawColumn screenWidth drawPile drawAllowed =
    elEmplacement screenWidth <|
        if drawAllowed then
            displayFCard (Just DrawCardFromDeckFrontend) FaceDown

        else if List.isEmpty drawPile then
            none

        else
            displayFCard Nothing FaceDown


displayOpponentRow : FPlayer -> Bool -> Bool -> Bool -> Maybe Int -> Element FrontendMsg
displayOpponentRow player isPlayerTurn isSwitchingCard isTamalouOwner maybeCardIndex =
    let
        attrsOwnTurn : List (Attribute FrontendMsg)
        attrsOwnTurn =
            if isPlayerTurn then
                [ Background.color yellow, Border.color yellow ]

            else
                [ Border.color blue ]

        switchingCardsAttrs : Maybe (Int -> CardClickMsg)
        switchingCardsAttrs =
            if isSwitchingCard && not isTamalouOwner then
                Just <| ChooseOpponentCardToSwitchFrontend player.sessionId

            else
                Nothing
    in
    row
        [ spacing 8 ]
        [ column ([ Font.size 11, Border.width 1, Border.rounded 8, padding 4, spacing 4 ] ++ attrsOwnTurn) <| List.map (el [ centerX ] << text) (String.split " " player.name)
        , row [ spacing 4, centerX, height fill ] <|
            reverseIndexedMap
                (\i c -> el [ rotate (2 * pi / 2) ] <| displayFCardSized (px 50) switchingCardsAttrs maybeCardIndex i c)
                (player.tableHand |> List.reverse)
        ]


reverseIndexedMap : (Int -> a -> b) -> List a -> List b
reverseIndexedMap f xs =
    List.indexedMap f xs |> List.reverse


displayRightOpponentColumn : FPlayer -> Bool -> Bool -> Bool -> Maybe Int -> Element FrontendMsg
displayRightOpponentColumn player isPlayerTurn isSwitchingCard isTamalouOwner maybeCardIndex =
    let
        attrsOwnTurn : List (Attribute FrontendMsg)
        attrsOwnTurn =
            if isPlayerTurn then
                [ Background.color yellow, Border.color yellow ]

            else
                [ Border.color blue ]

        switchingCardsMsg : Maybe (Int -> CardClickMsg)
        switchingCardsMsg =
            if isSwitchingCard && not isTamalouOwner then
                Just <| ChooseOpponentCardToSwitchFrontend player.sessionId

            else
                Nothing
    in
    column
        [ alignTop, height (fill |> Element.maximum 600) ]
        [ el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4 ] ++ attrsOwnTurn) <| text player.name
        , column [ spacing -22 ] <|
            reverseIndexedMap
                (\i c -> el [ rotate (3 * pi / 2) ] <| displayFCardSized (px 50) switchingCardsMsg maybeCardIndex i c)
                (player.tableHand |> List.reverse)
        ]


displayLeftOpponentColumn : FPlayer -> Bool -> Bool -> Bool -> Maybe Int -> Element FrontendMsg
displayLeftOpponentColumn player isPlayerTurn isSwitchingCard isTamalouOwner maybeCardIndex =
    let
        attrsOwnTurn : List (Attribute FrontendMsg)
        attrsOwnTurn =
            if isPlayerTurn then
                [ Background.color yellow, Border.color yellow ]

            else
                [ Border.color blue ]

        switchingCardsMsg : Maybe (Int -> CardClickMsg)
        switchingCardsMsg =
            if isSwitchingCard && not isTamalouOwner then
                Just <| ChooseOpponentCardToSwitchFrontend player.sessionId

            else
                Nothing
    in
    column
        [ alignTop, height fill ]
        [ el ([ Font.size 11, alignTop, Border.width 1, Border.rounded 8, padding 4, alignLeft ] ++ attrsOwnTurn) <| text player.name
        , column [ spacing -22 ] <|
            List.indexedMap
                (\i c -> el [ rotate (pi / 2) ] <| displayFCardSized (px 50) switchingCardsMsg maybeCardIndex i c)
                player.tableHand
        ]



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


displayOpponent : Maybe TamalouOwner -> Maybe SessionId -> Bool -> OpponentDisposition -> Maybe Int -> Element FrontendMsg
displayOpponent maybeTamalouOwner maybeSessionId switchingCard opponentDisposition maybeCardIndex =
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
            el [ alignLeft, alignTop, paddingEach { edges | left = 40, top = 12 } ] <| displayLeftOpponentColumn player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex

        TopLeftPlayer (Just player) ->
            el [ alignLeft ] <| displayOpponentRow player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex

        TopRightPlayer (Just player) ->
            el [ alignRight, paddingEach { edges | right = 32 } ] <| displayOpponentRow player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex

        RightPlayer (Just player) ->
            el [ alignRight, alignTop, paddingEach { edges | top = 12 } ] <| displayRightOpponentColumn player (isPlayerTurn player.sessionId) switchingCard (isTamalouOwner player.sessionId) maybeCardIndex

        _ ->
            none
