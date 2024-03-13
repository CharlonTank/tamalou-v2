module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Card
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
      , gameFrontend = FrontendWaitingForPlayers []
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



-- CardShuffled shuffledDeck ->
--     ( { model | deck = shuffledDeck }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ConnectedBack sessionId clientId frontendGame ->
            ( { model | gameFrontend = Debug.log "frontendGame" frontendGame, clientId = Just clientId, sessionId = Just sessionId }, Cmd.none )

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
        ]


grey : Color
grey =
    Element.rgb255 200 200 200


displayGame : FrontendModel -> Element FrontendMsg
displayGame model =
    case model.gameFrontend of
        FrontendWaitingForPlayers players ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                (text "Waiting for players"
                    :: List.map displayPlayerDebug players
                )

        FrontendGameInProgress drawPile discardPile players (TimerRunning timer) ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game in progress during timer"
                , displayTimer timer
                , displayPlayerView model.sessionId players
                ]

        FrontendGameInProgress drawPile discardPile players (PlayerToPlay sessionId) ->
            let
                currentPlayer =
                    List.Extra.find (\p -> Just p.sessionId == model.sessionId) players
            in
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game in progress after timer"
                , text sessionId
                , displayPlayerView model.sessionId players
                , if Maybe.map .sessionId currentPlayer == Just sessionId then
                    Input.button [] { onPress = Nothing, label = text "Draw" }

                  else
                    none
                ]

        FrontendGameEnded clientId ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game ended"
                , Element.text clientId
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


displayPlayerDebug : FrontendPlayer -> Element FrontendMsg
displayPlayerDebug player =
    column
        [ width fill, spacing 12 ]
        [ text player.name
        , row [ spacing 8 ] [ text "ClientId = ", text player.clientId ]
        , row [ spacing 8 ] [ text "SessionId = ", text player.sessionId ]
        , Card.displayCards player.hand
        ]


displayPlayerView : Maybe SessionId -> List FrontendPlayer -> Element FrontendMsg
displayPlayerView sessionId players =
    case List.Extra.find (\p -> Just p.sessionId == sessionId) players of
        Just player ->
            column []
                [ displayPlayerDebug player

                -- , column [] <| List.map displayPlayer players
                ]

        Nothing ->
            column []
                [ text "A skip je joue pas ?"
                , column [] <| List.map displayPlayerDebug players
                ]
