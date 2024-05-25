module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Delay
import Display.Admin exposing (displayAdmin)
import Display.Game exposing (game)
import Game exposing (BGame, BGameStatus(..), FGame(..), FGameInProgressStatus(..))
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
      , errors = []
      , admin = False
      , viewPort = { height = 0, width = 0 }
      , ready = False
      , maybeName = Nothing
      , chatInput = ""
      , chat = []
      , gameDisposition = NotCalculated
      , alreadyInAction = False
      , posix = Time.millisToPosix 0
      , route = route
      , games = []
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
            , Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (ChangeCurrentPlayerNameToBackend newName)
            )

        ImReadyFrontend ->
            ( { model
                | ready = True
                , chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, "Let's go I'm ready!" ) ]
              }
            , Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName ImReadyToBackend
            )

        ReStartGameFrontend fPlayer ->
            ( { model | ready = False }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (ReStartGameToBackend fPlayer) )

        TamalouFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName TamalouToBackend )

        PowerIsUsedFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName PowerIsUsedToBackend )

        PowerPassFrontend ->
            ( model, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName PowerIsNotUsedToBackend )

        ChangeChatInputFrontend newChatInput ->
            ( { model | chatInput = newChatInput }, Cmd.none )

        SendMessageFrontend ->
            ( { model | chatInput = "", chat = model.chat ++ [ ( Maybe.withDefault "" model.maybeName, model.chatInput ) ] }
            , Cmd.batch
                [ Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (SendMessageToBackend model.chatInput)
                , scrollToBottom "chatty"
                ]
            )

        CardClickMsg cardClickMsg ->
            case cardClickMsg of
                DrawCardFromDeckFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName DrawFromDrawPileToBackend )

                DrawFromDiscardPileFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName DrawFromDiscardPileToBackend )

                DiscardCardFrontend ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName DiscardCardInHandToBackend )

                CardClickReplacement cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (ReplaceCardInTableHandToBackend cardIndex) )

                DoubleCardFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (DoubleCardInTableHandToBackend cardIndex) )

                LookAtCardFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (LookAtCardInTableHandToBackend cardIndex) )

                ChooseOwnCardToSwitchFrontend cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (ChooseOwnCardToSwitchToBackend cardIndex) )

                ChooseOpponentCardToSwitchFrontend sessionId cardIndex ->
                    ( { model | alreadyInAction = True }, Lamdera.sendToBackend <| ActionFromGameToBackendWrapper roomName (ChooseOpponentCardToSwitchToBackend ( sessionId, cardIndex )) )

        Frame posix ->
            ( updateEveryTimelineOnFrame model posix, Cmd.none )

        UpdateFGamePostAnimationFrontend fGame playerActionAnimation ->
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

        BackHomeFrontend ->
            ( model, Nav.load "/" )


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
                    , Lamdera.sendToBackend (ActionFromHomeToBackendWrapper GetGamesToBackend)
                    )

                Router.Admin ->
                    ( { model | clientId = Just clientId, sessionId = Just sessionId, admin = True }
                    , Lamdera.sendToBackend ConnectToAdminToBackend
                    )

                Router.Game roomName ->
                    ( { model | clientId = Just clientId, sessionId = Just sessionId }
                    , Lamdera.sendToBackend (ActionFromGameToBackendWrapper roomName ConnectGameToBackend)
                    )

        UpdateGamesToFrontend games ->
            ( { model | games = games }, Cmd.none )


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
        |> List.filter (\p -> maybeSessionId /= Just p.sessionId)


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
        |> List.find (\p -> maybeSessionId == Just p.sessionId)
        |> Maybe.map .name


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tamalou!"
    , body =
        [ layout
            [ behindContent <| image [ height fill ] { description = "background", onLoad = Nothing, source = "/background.png" }
            , Font.size 12
            , height fill
            , scrollableAll
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
        , row [ spacing 12, width shrink, centerX ]
            [ Input.text [ centerX, width <| px 200, Font.size 24, rounded 8 ]
                { label = Input.labelHidden "Room name"
                , onChange = ChangeRoomNameFrontend
                , placeholder = Just "keuh-kli"
                , text = model.roomName
                }
            , el
                [ centerX
                , Input.button <| JoinRoomGameFrontend model.roomName
                , rounded 8

                -- , border 2
                , paddingXY 16 0
                , background green
                , Font.color white

                -- , borderColor strongerBlue
                , Font.size 24
                , height <| px 53
                ]
              <|
                el [ centerY ] <|
                    text "Join!"
            ]
        , displayGames model.games
        ]


displayGames : List BGame -> Element FrontendMsg
displayGames games =
    let
        displayCategory : String -> List BGame -> Element FrontendMsg
        displayCategory title gamesList =
            if List.isEmpty gamesList then
                none

            else
                column []
                    [ el [ centerX, Font.bold ] <| text title
                    , column [ spacing 10 ] (List.map displayGame gamesList)
                    ]

        finishedGames : List BGame
        finishedGames =
            List.filter
                (\game ->
                    case game.status of
                        BGameEnded _ ->
                            True

                        _ ->
                            False
                )
                games

        ongoingGames : List BGame
        ongoingGames =
            List.filter
                (\game ->
                    case game.status of
                        BGameInProgress _ _ _ _ _ _ _ ->
                            True

                        _ ->
                            False
                )
                games

        waitingGames : List BGame
        waitingGames =
            List.filter
                (\game ->
                    case game.status of
                        BWaitingForPlayers _ ->
                            True

                        _ ->
                            False
                )
                games
    in
    column
        [ spacing 20
        , Font.size 16
        ]
        [ displayCategory "Available Rooms" waitingGames
        , displayCategory "Ongoing Games" ongoingGames
        , displayCategory "Finished Games" finishedGames
        ]


displayGame : BGame -> Element FrontendMsg
displayGame game =
    row
        [ spacing 5
        , padding 10
        , border 1
        , rounded 8
        , widthMax 600
        , centerX
        ]
        [ el [] <| text game.name
        , row [ contentCenterX, width <| portion 2 ]
            (case game.status of
                BWaitingForPlayers players ->
                    [ List.map (\player -> text <| getOnlyTheFirstNbCharactersButFullWords 10 player.name) players
                        |> List.intersperse (el [ width shrink, alignLeft ] <| text "-")
                        |> row [ width shrink, alignLeft, spacing 4 ]
                    , el [ alignRight ] <| text <| (String.fromInt (List.length players) ++ "/5")
                    ]
                        ++ (if List.length players < 5 then
                                [ el [ alignRight, Input.button <| JoinRoomGameFrontend game.name, rounded 8, padding 8, background green, Font.color white ] <| text "Join!" ]

                            else
                                [ el [ alignRight, rounded 8, padding 8, background lightGrey, Font.color strongerGrey ] <| text "Full" ]
                           )

                BGameInProgress _ _ _ _ _ _ _ ->
                    [ el [ centerX ] <| text "Game in progress", text "Spectate! (coming soon)" ]

                BGameEnded orderedPlayers ->
                    [ orderedPlayers
                        |> List.head
                        |> Maybe.map (\( player, score ) -> player.name ++ " won with " ++ String.fromInt score ++ " points")
                        |> Maybe.withDefault "Game ended"
                        |> text
                        |> el [ centerX ]
                    ]
            )
        ]


getOnlyTheFirstNbCharactersButFullWords : Int -> String -> String
getOnlyTheFirstNbCharactersButFullWords nb str =
    List.foldl
        (\word ( remaining, acc ) ->
            if String.isEmpty acc then
                if String.length word > nb then
                    ( 0, String.left (nb - 3) word ++ "..." )

                else
                    ( remaining - String.length word, word )

            else
                let
                    newAcc : String
                    newAcc =
                        acc ++ " " ++ word
                in
                if String.length newAcc > nb then
                    ( 0, acc )

                else
                    ( remaining - String.length word - 1, newAcc )
        )
        ( nb, "" )
        (String.words str)
        |> Tuple.second
