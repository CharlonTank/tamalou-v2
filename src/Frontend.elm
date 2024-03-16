module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Card exposing (FCard(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html
import Html.Attributes as Attr
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Random
import Random.List as Random
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , gameFrontend = FWaitingForPlayers []
      , clientId = Nothing
      , sessionId = Nothing
      , urlPath = url.path
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg ({ urlPath } as model) =
    case msg of
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

        UrlChanged url ->
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
        [ width fill, height fill, padding 20, spacing 20, Background.color grey, scrollbars ]
        [ displayGame model

        -- , displayGameDebug model
        ]


grey : Color
grey =
    Element.rgb255 200 200 200


type CardClickEvent
    = CardClickReplacement
    | CardClickDouble


displayGame : FrontendModel -> Element FrontendMsg
displayGame model =
    case model.gameFrontend of
        FWaitingForPlayers players ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                (text "Waiting for players" :: List.map displayPlayerDebug players)

        FGameInProgress hand drawPile discardPile players (FTimerRunning timer) ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game in progress during timer"
                , displayTimer timer
                , displayPlayerView model.sessionId players hand Nothing
                ]

        FGameInProgress hand drawPile discardPile players (FPlayerToPlay sessionId FWaitingPlayerDraw) ->
            let
                currentPlayer =
                    List.Extra.find (\p -> Just p.sessionId == model.sessionId) players

                cardClickEvent =
                    if List.isEmpty discardPile then
                        Nothing

                    else
                        Just CardClickDouble
            in
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ text "Game in progress after timer"
                , currentPlayer |> Maybe.map .name |> Maybe.withDefault "" |> text
                , text sessionId
                , displayPlayerView model.sessionId players hand cardClickEvent
                ]

        FGameInProgress hand drawPile discardPile players (FPlayerToPlay sessionId (FPlayerHasDraw fCard)) ->
            let
                currentPlayer =
                    List.Extra.find (\p -> Just p.sessionId == model.sessionId) players

                cardClickEvent =
                    if List.isEmpty discardPile then
                        Nothing

                    else
                        Just CardClickDouble
            in
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ text "Game in progress after timer"
                , currentPlayer |> Maybe.map .name |> Maybe.withDefault "" |> text
                , text sessionId
                , displayCard FaceDown
                , displayPlayerView model.sessionId players hand cardClickEvent
                ]

        FGameInProgress hand drawPile discardPile players (FYourTurn FWaitingPlayerDraw) ->
            let
                cardClickEvent =
                    if List.isEmpty discardPile then
                        Nothing

                    else
                        Just CardClickDouble
            in
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ text "Game in progress after timer"
                , text "Your turn"
                , row [ spacing 16 ]
                    [ Input.button [] { onPress = Just DrawCardFromDeckFrontend, label = text "Draw the drawPile" }
                    , displayDiscardCards discardPile True
                    , Input.button [] { onPress = Just TamalouFrontend, label = text "Tamalou" }
                    ]
                , displayPlayerView model.sessionId players hand cardClickEvent
                ]

        FGameInProgress hand drawPile discardPile players (FYourTurn (FPlayerHasDraw fCard)) ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ text "Game in progress after timer"
                , text "Your turn"
                , displayCard fCard
                , row [ spacing 16 ]
                    [ Input.button [] { onPress = Just DiscardCardFrontend, label = text "Discard" }
                    , displayDiscardCards discardPile False

                    -- , Input.button [] { onPress = Just Tamalou, label = text "Tamalou" }
                    ]
                , displayPlayerView model.sessionId players hand (Just CardClickReplacement)
                ]

        FGameEnded clientId ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game ended"
                , Element.text clientId
                ]

        FGameAlreadyStartedWithoutYou ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Sorry! The game already started without you, if you wanna play you can just go in a new url"
                ]


displayDiscardCards : DiscardPile -> Bool -> Element FrontendMsg
displayDiscardCards discardPile canDrawCard =
    case discardPile of
        [] ->
            text "X"

        head :: _ ->
            column [ spacing 4 ]
                [ displayCard (FaceUp head)
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

        -- , displayCards player.tableHand
        ]


displayPlayerView : Maybe SessionId -> List FPlayer -> FTableHand -> Maybe CardClickEvent -> Element FrontendMsg
displayPlayerView sessionId players tableHand maybeCardClick =
    -- case List.Extra.find (\p -> Just p.sessionId == sessionId) players of
    --     Just player ->
    column []
        [ displayCards tableHand maybeCardClick

        -- , column [] <| List.map displayPlayer players
        ]



-- Nothing ->
--     column []
--         [ text "A skip je joue pas ?"
--         -- , column [] <| List.map displayPlayerDebug players
--         ]


displayGameDebug : FrontendModel -> Element FrontendMsg
displayGameDebug model =
    column
        [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
        [ text "Game debug"

        -- , text <| Debug.toString model
        ]


displayCards : List FCard -> Maybe CardClickEvent -> Element FrontendMsg
displayCards cards maybeCardClickEvent =
    wrappedRow [ spacing 12 ] (List.indexedMap (onClickCard maybeCardClickEvent displayCard) cards)


onClickCard : Maybe CardClickEvent -> (FCard -> Element FrontendMsg) -> Int -> FCard -> Element FrontendMsg
onClickCard maybeCardClickEvent tag index card =
    case maybeCardClickEvent of
        Nothing ->
            tag card

        Just CardClickDouble ->
            Element.el [ Events.onClick (DoubleCardFrontend index) ] (tag card)

        Just CardClickReplacement ->
            Element.el [ Events.onClick (ReplaceCardInFrontend index) ] (tag card)


displayCard : FCard -> Element msg
displayCard frontendCard =
    image [ Border.rounded 100, width <| px 128 ] <|
        case frontendCard of
            FaceUp card ->
                { src = "/cardImages/" ++ Card.toString card ++ ".png", description = Card.toString card }

            FaceDown ->
                { src = "/cardImages/BackCovers/Pomegranate.png", description = "back" }
