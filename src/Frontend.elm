module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Delay
import Display.Admin exposing (displayAdmin)
import Display.Game exposing (game)
import Game exposing (FGame(..), FGameInProgressStatus(..))
import Lamdera exposing (SessionId)
import List.Extra as List
import Palette.Color exposing (..)
import Player exposing (FPlayer, FPlayerToPlayStatus(..))
import Positioning.Animate exposing (animDuration, animatePlayerAction, updateEveryTimelineOnFrame)
import Positioning.Helpers exposing (scrollToBottom)
import Positioning.Positioning exposing (..)
import Router
import Task
import Time
import Types exposing (..)
import Ui exposing (..)
import Ui.Font as Font
import Ui.Input as Input
import Url
import Utils.Ui exposing (..)


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
subscriptions fModel =
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotWindowSize { height = h, width = w })
        , case fModel.fGame of
            Just (FGameInProgress _ _ _ _ _ _) ->
                Browser.Events.onAnimationFrame Frame

            _ ->
                Sub.none
        ]


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        route : Router.Route
        route =
            Router.parseUrl url
    in
    ( { key = key
      , device = Device Phone Landscape
      , fGame = Nothing
      , roomName =
            case route of
                Router.Game roomName ->
                    roomName

                _ ->
                    ""
      , clientId = Nothing
      , sessionId = Nothing

      --   , urlPath = url.path
      , errors = []
      , admin = False
      , viewPort = { height = 0, width = 0 }
      , ready = False
      , maybeName = Nothing
      , chatInput = ""
      , chat = []
      , gameDisposition = NotCalculated

      --   , animationState = Anim.init
      , alreadyInAction = False
      , posix = Time.millisToPosix 0
      , route = route
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
update msg ({ roomName } as model) =
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
                , gameDisposition =
                    case model.fGame of
                        Just fGame ->
                            Calculated <| calculateGameDisposition viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)

                        Nothing ->
                            NotCalculated
              }
            , Cmd.none
            )

        ChangeCurrentPlayerNameFrontend newName ->
            ( { model | maybeName = Just newName }
            , Lamdera.sendToBackend <| ActionFromGameToBackend roomName (ChangeCurrentPlayerNameToBackend newName)
            )

        ImReadyFrontend ->
            ( { model
                | ready = True
                , chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, "Let's go I'm ready!" ) ]
              }
            , Lamdera.sendToBackend <| ActionFromGameToBackend roomName ImReadyToBackend
            )

        ReStartGameFrontend fPlayer ->
            ( { model | ready = False }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName (ReStartGameToBackend fPlayer) )

        TamalouFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend roomName TamalouToBackend )

        PowerIsUsedFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend roomName PowerIsUsedToBackend )

        PowerPassFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackend roomName PowerIsNotUsedToBackend )

        ChangeChatInputFrontend newChatInput ->
            ( { model | chatInput = newChatInput }, Cmd.none )

        SendMessageFrontend ->
            ( { model | chatInput = "", chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, model.chatInput ) ] }
            , Cmd.batch
                [ Lamdera.sendToBackend <| ActionFromGameToBackend roomName (SendMessageToBackend model.chatInput)
                , scrollToBottom "chatty"
                ]
            )

        CardClickMsg cardClickMsg ->
            case cardClickMsg of
                DrawCardFromDeckFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName DrawFromDrawPileToBackend )

                DrawFromDiscardPileFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName DrawFromDiscardPileToBackend )

                DiscardCardFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName DiscardCardInHandToBackend )

                CardClickReplacement cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName (ReplaceCardInTableHandToBackend cardIndex) )

                DoubleCardFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName (DoubleCardInTableHandToBackend cardIndex) )

                LookAtCardFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName (LookAtCardInTableHandToBackend cardIndex) )

                ChooseOwnCardToSwitchFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName (ChooseOwnCardToSwitchToBackend cardIndex) )

                ChooseOpponentCardToSwitchFrontend sessionId cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend roomName (ChooseOpponentCardToSwitchToBackend ( sessionId, cardIndex )) )

        -- AnimMsg animMsg ->
        --     let
        --         ( newAnimationState, cmds ) =
        --             Anim.update AnimMsg animMsg model.animationState
        --     in
        --     ( { model | animationState = newAnimationState }
        --     , cmds
        --     )
        Frame posix ->
            ( updateEveryTimelineOnFrame model posix, Cmd.none )

        UpdateFGamePostAnimationFrontend fGame playerActionAnimation ->
            -- based on the playerAction, let's update the model's gameDisposition
            ( { model
                | fGame = Just fGame
                , maybeName =
                    case model.maybeName of
                        Just _ ->
                            model.maybeName

                        Nothing ->
                            getMyName model.sessionId fGame
                , gameDisposition =
                    case model.gameDisposition of
                        NotCalculated ->
                            Calculated <| calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)

                        Calculated oldPositions ->
                            Calculated <| calculateGameDispositionBasedOnAnimation model.sessionId model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame) oldPositions playerActionAnimation
                , alreadyInAction = False
              }
            , Cmd.none
            )

        ChangeRoomNameFrontend newRoomName ->
            ( { model | roomName = newRoomName }, Cmd.none )

        JoinRoomGameFrontend roomNameToJoin ->
            ( model
            , Nav.load roomNameToJoin
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateAdminToFrontend errors ->
            ( { model | errors = errors }, Cmd.none )

        UpdateGameStatusToFrontend fGame maybePlayerActionAnimation ->
            let
                newGameDisposition : Positions
                newGameDisposition =
                    calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
            in
            case maybePlayerActionAnimation of
                Just playerAction ->
                    ( { model
                        | maybeName =
                            case model.maybeName of
                                Just _ ->
                                    model.maybeName

                                Nothing ->
                                    getMyName model.sessionId fGame
                      }
                        |> animatePlayerAction playerAction newGameDisposition
                    , Delay.after (round animDuration) (UpdateFGamePostAnimationFrontend fGame playerAction)
                    )

                Nothing ->
                    ( { model
                        | fGame = Just fGame
                        , maybeName =
                            case model.maybeName of
                                Just _ ->
                                    model.maybeName

                                Nothing ->
                                    getMyName model.sessionId fGame
                        , gameDisposition = Calculated newGameDisposition
                      }
                    , Cmd.none
                      -- , Delay.after (round animDuration) (UpdateFGamePostAnimationFrontend fGame NoPlayerAction)
                    )

        UpdateGameAndChatToFrontend ( fGame, chat ) ->
            ( { model
                | fGame = Just fGame
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
            case model.route of
                Router.Home ->
                    ( { model | clientId = Just clientId, sessionId = Just sessionId }
                    , Cmd.none
                    )

                Router.Admin ->
                    ( { model | clientId = Just clientId, sessionId = Just sessionId, admin = True }
                    , Lamdera.sendToBackend ConnectToAdminToBackend
                    )

                Router.Game roomName ->
                    ( { model | clientId = Just clientId, sessionId = Just sessionId }
                    , Lamdera.sendToBackend (ActionFromGameToBackend roomName ConnectToBackend)
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


getOwnedCards : FGame -> List FCard
getOwnedCards fGame =
    case fGame of
        FGameInProgress _ _ _ _ _ (FYourTurn (FPlayerDisplayTamalouFailure hand _)) ->
            List.map FaceUp hand

        FGameInProgress _ hand _ _ _ _ ->
            hand

        _ ->
            []


getMyName : Maybe SessionId -> FGame -> Maybe String
getMyName maybeSessionId fGame =
    fPlayersFromFGame fGame
        |> List.find (\player -> maybeSessionId == Just player.sessionId)
        |> Maybe.map .name


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tamalou!"
    , body =
        [ layout
            [ behindContent <| image [ height fill ] { description = "background", onLoad = Nothing, source = "/background.png" }
            , Font.size 12
            , height fill
            ]
          <|
            case ( model.route, model.admin, model.gameDisposition ) of
                ( Router.Home, _, _ ) ->
                    displayHome model

                ( Router.Admin, True, _ ) ->
                    displayAdmin model

                ( Router.Game _, _, NotCalculated ) ->
                    column
                        [ height fill ]
                        [ text "Loading..." ]

                ( Router.Game _, _, Calculated positions ) ->
                    column
                        [ height fill ]
                        [ game model positions ]

                _ ->
                    text "401 Unauthorized"
        ]
    }


displayHome : FrontendModel -> Element FrontendMsg
displayHome model =
    column
        [ padding 20
        , height fill
        , Font.center
        , spacing 20
        , Font.size 16
        ]
        [ el [ Font.size 20, centerX ] <| text "Welcome on Tamalou!"
        , text "Enter the room you want to join here!"
        , Input.text [ centerX, width <| px 200, Font.size 24 ]
            { label = Input.labelHidden "Room name"
            , onChange = ChangeRoomNameFrontend
            , placeholder = Just "keuh-kli"
            , text = model.roomName
            }
        , el
            [ centerX
            , Input.button <| JoinRoomGameFrontend model.roomName
            , rounded 8
            , border 2
            , paddingXY 4 2
            , background green
            , borderColor strongerBlue
            ]
          <|
            text "Join the room"
        ]
