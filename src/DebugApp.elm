module DebugApp exposing (..)

import Card exposing (Rank(..))
import Http
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import Task
import Time
import Types exposing (BGameInProgressStatus(..), BGameStatus(..), BPlayer, BPlayerToPlayStatus(..), Counter(..), LookACardStatus(..), Switch2CardsStatus(..))
import Utils.String as String



-- backend :
--     backendMsg
--     -> String
--     ->
--         { init : ( backendModel, Cmd backendMsg )
--         , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
--         , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
--         , subscriptions : backendModel -> Sub backendMsg
--         }
--     ->
--         { init : ( backendModel, Cmd backendMsg )
--         , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
--         , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
--         , subscriptions : backendModel -> Sub backendMsg
--         }
-- backend backendNoOp sessionName { init, update, updateFromFrontend, subscriptions } =
--     { init =
--         let
--             ( model, cmd ) =
--                 init
--         in
--         ( model
--         , Cmd.batch
--             [ cmd
--             , sendToViewer
--                 backendNoOp
--                 (Init { sessionName = sessionName, model = Debug.toString model })
--             ]
--         )
--     , update =
--         \msg model ->
--             let
--                 ( newModel, cmd ) =
--                     update msg model
--             in
--             ( newModel
--             , Cmd.batch
--                 [ cmd
--                 , if backendNoOp == msg then
--                     Cmd.none
--                   else
--                     sendToViewer
--                         backendNoOp
--                         (Update
--                             { sessionName = sessionName
--                             , msg = Debug.toString msg
--                             , newModel = Debug.toString newModel
--                             }
--                         )
--                 ]
--             )
--     , updateFromFrontend =
--         \sessionId clientId msg model ->
--             let
--                 ( newModel, cmd ) =
--                     updateFromFrontend sessionId clientId msg model
--             in
--             ( newModel
--             , Cmd.batch
--                 [ cmd
--                 , sendToViewer
--                     backendNoOp
--                     (UpdateFromFrontend
--                         { sessionName = sessionName
--                         , msg = Debug.toString msg
--                         , newModel = Debug.toString newModel
--                         , sessionId = sessionId
--                         , clientId = clientId
--                         }
--                     )
--                 ]
--             )
--     , subscriptions = subscriptions
--     }


type DataType
    = Init { sessionName : String, model : String }
    | Update { sessionName : String, msg : String, newModel : String }
    | UpdateFromFrontend { sessionName : String, msg : String, newModel : String, sessionId : String, clientId : String }


sendToViewer : msg -> DataType -> Cmd msg
sendToViewer backendNoOp data =
    Time.now
        |> Task.andThen
            (\time ->
                Http.task
                    { method = "POST"
                    , headers = []
                    , url = "http://localhost:8001/https://backend-debugger.lamdera.app/_r/data"
                    , body = Http.jsonBody (encodeDataType time data)
                    , resolver = Http.bytesResolver (\_ -> Ok ())
                    , timeout = Just 10000
                    }
            )
        |> Task.attempt (\_ -> backendNoOp)


encodeTime : Time.Posix -> Json.Encode.Value
encodeTime time =
    Time.posixToMillis time |> Json.Encode.int


encodeDataType : Time.Posix -> DataType -> Json.Encode.Value
encodeDataType time data =
    Json.Encode.list
        identity
        (case data of
            Init { sessionName, model } ->
                [ Json.Encode.int 0
                , Json.Encode.string sessionName
                , Json.Encode.string model
                , Json.Encode.null
                , encodeTime time
                ]

            Update { sessionName, msg, newModel } ->
                [ Json.Encode.int 1
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.null
                , encodeTime time
                ]

            UpdateFromFrontend { sessionName, msg, newModel, sessionId, clientId } ->
                [ Json.Encode.int 2
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.string sessionId
                , Json.Encode.string clientId
                , Json.Encode.null
                , encodeTime time
                ]
        )


bGameInProgressLogs : String -> BGameStatus -> String
bGameInProgressLogs msg status =
    case status of
        BWaitingForPlayers players ->
            msg ++ ": BWaitingForPlayers: " ++ (List.map bPlayerLogs players |> String.join ", ")

        BGameInProgress _ _ _ players gameInProgressStatus lastMoveIsDouble canUsePowerFromLastPlayer ->
            msg ++ "BGameInProgress: " ++ (List.map bPlayerLogs players |> String.join ", ") ++ ", gameInProgressStatus: " ++ bGameInProgressStatusLogs gameInProgressStatus ++ ", lastMoveIsDouble: " ++ String.fromBool lastMoveIsDouble ++ " canUsePowerFromLastPlayer: " ++ String.fromBool canUsePowerFromLastPlayer

        BGameEnded players ->
            msg ++ "BGameEnded: " ++ (List.map bPlayerLogs players |> String.join ", ")


bGameInProgressStatusLogs : BGameInProgressStatus -> String
bGameInProgressStatusLogs status =
    case status of
        BStartTimerRunning counter ->
            "BStartTimerRunning: " ++ counterToString counter

        BPlayerToPlay player playerToPlayStatus ->
            "BPlayerToPlay: " ++ bPlayerLogs player ++ ", playerToPlayStatus: " ++ bPlayerToPlayStatusLogs playerToPlayStatus

        BEndTimerRunning counter ->
            "BEndTimerRunning: " ++ counterToString counter


bPlayerToPlayStatusLogs : BPlayerToPlayStatus -> String
bPlayerToPlayStatusLogs status =
    case status of
        BWaitingPlayerAction maybePower ->
            "BWaitingPlayerAction: "
                ++ (case maybePower of
                        Just power ->
                            "Just " ++ Card.powerToString power

                        Nothing ->
                            "Nothing"
                   )

        BPlayerHasDraw card ->
            "BPlayerHasDraw: " ++ Card.toString card

        BPlayerHasDiscard power ->
            "BPlayerHasDiscard: " ++ Card.powerToString power

        BPlayerLookACard lookACardStatus ->
            "BPlayerLookACard: " ++ lookACardStatusLogs lookACardStatus

        BPlayerSwitch2Cards switch2CardsStatus ->
            "BPlayerSwitch2Cards: " ++ switch2CardsStatusLogs switch2CardsStatus


switch2CardsStatusLogs : Switch2CardsStatus -> String
switch2CardsStatusLogs status =
    case status of
        ChooseOwnCardToSwitch ->
            "ChooseOwnCardToSwitch"

        OwnCardChosen index ->
            "OwnCardChosen: " ++ String.fromInt index


lookACardStatusLogs : LookACardStatus -> String
lookACardStatusLogs status =
    case status of
        ChooseCardToLook ->
            "ChooseCardToLook"

        LookingACard counter ->
            "LookingACard: " ++ counterToString counter


counterToString : Counter -> String
counterToString counter =
    case counter of
        Types.Five ->
            "Five"

        Types.Four ->
            "Four"

        Types.Three ->
            "Three"

        Types.Two ->
            "Two"

        Types.One ->
            "One"

        Types.Zero ->
            "Zero"


bPlayerLogs : BPlayer -> String
bPlayerLogs player =
    "name: " ++ player.name ++ " clientId: " ++ player.clientId ++ " sessionId: " ++ player.sessionId ++ " ready: " ++ String.fromBool player.ready
