module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Card
import Element exposing (..)
import Element.Background as Background
import Html
import Html.Attributes as Attr
import Lamdera exposing (ClientId)
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

        ConnectedBack clientId frontendGame ->
            ( { model | gameFrontend = frontendGame, clientId = Just clientId }, Cmd.none )

        UpdateGame game ->
            ( { model | gameFrontend = game }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [ width fill, height fill ] <| displayModel model
        ]
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
                ([ text "Waiting for players"
                 ]
                    ++ List.map displayPlayer players
                )

        -- | FrontendGameInProgress DrawPile DiscardPile (List FrontendPlayer)
        FrontendGameInProgress drawPile discardPile players ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game in progress"
                , displayPlayerView model.clientId players
                ]

        -- FrontendGameInProgress drawPile discardPile players ->
        --     column
        --         [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
        --         ([ Element.text "Game in progress"
        --          ]
        --             ++ List.map displayPlayer players
        --         )
        FrontendGameEnded clientId ->
            column
                [ width fill, height fill, spacing 20, Background.color grey, scrollbars ]
                [ Element.text "Game ended"
                , Element.text clientId
                ]


displayPlayer : FrontendPlayer -> Element FrontendMsg
displayPlayer player =
    column
        [ width fill, spacing 12 ]
        [ text player.name
        , text player.clientId
        , Card.displayCards player.hand
        ]


displayPlayerView : Maybe ClientId -> List FrontendPlayer -> Element FrontendMsg
displayPlayerView clientId players =
    case List.filter (\player -> Just player.clientId == clientId) players of
        [ player ] ->
            displayPlayer player

        _ ->
            Element.text "Player not found"
