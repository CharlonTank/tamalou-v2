module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard)
import Delay
import Display.Admin exposing (displayAdmin)
import Display.Game exposing (game)
import Lamdera exposing (SessionId)
import List.Extra
import Positioning.Animate exposing (animDuration, animatePlayerAction, updateEveryTimelineOnFrame)
import Positioning.Helpers exposing (scrollToBottom)
import Positioning.Positioning exposing (..)
import Task
import Time
import Types exposing (..)
import Ui exposing (..)
import Ui.Font as Font
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
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotWindowSize { height = h, width = w })
        , Browser.Events.onAnimationFrame Frame
        ]


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
      , gameDisposition = NotCalculated

      --   , animationState = Anim.init
      , alreadyInAction = False
      , posix = Time.millisToPosix 0
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
                , gameDisposition = Calculated <| calculateGameDisposition viewPort (fPlayersFromFGame model.fGame |> getOpponents model.sessionId) (getOwnedCards model.fGame)
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
                    ( { model | alreadyInAction = True }
                    , Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawFromDrawPileToBackend
                    )

                DrawFromDiscardPileFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DrawFromDiscardPileToBackend )

                DiscardCardFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath DiscardCardInHandToBackend )

                CardClickReplacement cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ReplaceCardInTableHandToBackend cardIndex) )

                DoubleCardFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (DoubleCardInTableHandToBackend cardIndex) )

                LookAtCardFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (LookAtCardInTableHandToBackend cardIndex) )

                ChooseOwnCardToSwitchFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChooseOwnCardToSwitchToBackend cardIndex) )

                ChooseOpponentCardToSwitchFrontend sessionId cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackend urlPath (ChooseOpponentCardToSwitchToBackend ( sessionId, cardIndex )) )

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

        UpdateFGamePostAnimationFrontend fGame _ ->
            -- based on the playerAction, let's update the model's gameDisposition
            ( { model
                | fGame = fGame
                , maybeName =
                    case model.maybeName of
                        Just _ ->
                            model.maybeName

                        Nothing ->
                            getMyName model.sessionId fGame
                , gameDisposition = Calculated <| calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
                , alreadyInAction = False

                -- , gameDisposition = calculateGameDispositionParts model.viewPort playerAction (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
              }
            , Cmd.none
            )


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
                    let
                        newGameDisposition : Positions
                        newGameDisposition =
                            calculateGameDisposition model.viewPort (fPlayersFromFGame fGame |> getOpponents model.sessionId) (getOwnedCards fGame)
                    in
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
                        | maybeName =
                            case model.maybeName of
                                Just _ ->
                                    model.maybeName

                                Nothing ->
                                    getMyName model.sessionId fGame
                      }
                    , Delay.after (round animDuration) (UpdateFGamePostAnimationFrontend fGame NoPlayerAction)
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


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tamalou!"
    , body =
        [ layout
            [ behindContent <| image [ height fill ] { description = "background", source = "/background.png" }
            , Font.size 12
            , height fill
            ]
          <|
            case model.gameDisposition of
                NotCalculated ->
                    none

                Calculated positions ->
                    column
                        [ height fill ]
                        [ if model.admin then
                            displayAdmin model

                          else
                            game model positions
                        ]
        ]
    }
