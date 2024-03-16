module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Card exposing (FCard(..), displayCard, displayCards)
import Element exposing (..)
import Element.Background as Background
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
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
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

        DrawCardFrontend ->
            ( model, Lamdera.sendToBackend DrawCardFromDrawPileToBackend )

        TamalouFrontend ->
            ( model, Debug.todo "Tamalou not implemented" )

        DiscardCardFrontend ->
            ( model, Lamdera.sendToBackend DiscardCardToBackend )



-- CardShuffled shuffledDeck ->
--     ( { model | deck = shuffledDeck }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ConnectedBack sessionId clientId frontendGame ->
            ( { model | clientId = Just clientId, sessionId = Just sessionId }, Cmd.none )

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
        , displayGameDebug model
        ]


grey : Color
grey =
    Element.rgb255 200 200 200


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
                , displayPlayerView model.sessionId players hand
                ]

        FGameInProgress hand drawPile discardPile players (FPlayerToPlay sessionId FWaitingPlayerDraw) ->
            let
                currentPlayer =
                    List.Extra.find (\p -> Just p.sessionId == model.sessionId) players
            in
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game in progress after timer"
                , currentPlayer |> Maybe.map .name |> Maybe.withDefault "" |> text
                , text sessionId
                , if Maybe.map .sessionId currentPlayer == Just sessionId then
                    row [ spacing 16 ]
                        [ Input.button [] { onPress = Just DrawCardFrontend, label = text "Draw the drawPile" }
                        , displayDiscardCards discardPile True
                        , Input.button [] { onPress = Just TamalouFrontend, label = text "Tamalou" }
                        ]

                  else
                    row [ spacing 16 ]
                        [ displayDiscardCards discardPile False
                        ]
                , displayPlayerView model.sessionId players hand
                ]

        FGameInProgress hand drawPile discardPile players (FPlayerToPlay sessionId (FPlayerHasDraw fCard)) ->
            let
                currentPlayer =
                    List.Extra.find (\p -> Just p.sessionId == model.sessionId) players
            in
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ text "Game in progress after timer"
                , currentPlayer |> Maybe.map .name |> Maybe.withDefault "" |> text
                , text sessionId
                , displayCard fCard
                , if Maybe.map .sessionId currentPlayer == Just sessionId then
                    row [ spacing 16 ]
                        [ Input.button [] { onPress = Just DiscardCardFrontend, label = text "Discard" }
                        , displayDiscardCards discardPile True

                        -- , Input.button [] { onPress = Just Tamalou, label = text "Tamalou" }
                        ]

                  else
                    row [ spacing 16 ]
                        [ displayDiscardCards discardPile False
                        ]
                , displayPlayerView model.sessionId players hand
                ]

        FGameEnded clientId ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game ended"
                , Element.text clientId
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
                    Input.button [] { onPress = Just DrawCardFrontend, label = text "Draw the discardPile" }

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
        , Card.displayCards player.hand
        ]


displayPlayerView : Maybe SessionId -> List FPlayer -> FHand -> Element FrontendMsg
displayPlayerView sessionId players hand =
    case List.Extra.find (\p -> Just p.sessionId == sessionId) players of
        Just player ->
            column []
                [ Card.displayCards hand

                -- , column [] <| List.map displayPlayer players
                ]

        Nothing ->
            column []
                [ text "A skip je joue pas ?"
                , column [] <| List.map displayPlayerDebug players
                ]


displayGameDebug : FrontendModel -> Element FrontendMsg
displayGameDebug model =
    column
        [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
        [ text "Game debug"
        , text <| Debug.toString model
        ]
