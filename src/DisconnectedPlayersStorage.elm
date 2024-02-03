module DisconnectedPlayersStorage exposing (Storage, DisconnectedPlayer, addDisconnectedPlayer, removePlayer, isPlayerDisconnected, removeExpiredEntries)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Time exposing (Time)

type alias DisconnectedPlayer =
    { clientId : ClientId
    , name : String
    , disconnectionTime : Time
    }

type alias Storage =
    Dict ClientId DisconnectedPlayer

addDisconnectedPlayer : DisconnectedPlayer -> Storage -> Storage
addDisconnectedPlayer player storage =
    Dict.insert player.clientId player storage

removePlayer : ClientId -> Storage -> Storage
removePlayer clientId storage =
    Dict.remove clientId storage

isPlayerDisconnected : ClientId -> Storage -> Bool
isPlayerDisconnected clientId storage =
    Dict.member clientId storage

removeExpiredEntries : Time -> Storage -> Storage
removeExpiredEntries currentTime storage =
    Dict.filter (\_ player -> Time.diff currentTime player.disconnectionTime < Time.minutes 5) storage
