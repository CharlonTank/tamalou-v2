module DisconnectedPlayersStorage exposing (addPlayer, isPlayerDisconnected, removeExpiredEntries, removePlayer)

import Dict exposing (Dict)
import Time exposing (Posix)

type alias ClientId =
    String

type alias PlayerInfo =
    { name : String
    , disconnectedAt : Posix
    }

type alias Storage =
    Dict ClientId PlayerInfo

addPlayer : ClientId -> String -> Posix -> Storage -> Storage
addPlayer clientId name currentTime storage =
    Dict.insert clientId { name = name, disconnectedAt = currentTime } storage

removePlayer : ClientId -> Storage -> Storage
removePlayer clientId storage =
    Dict.remove clientId storage

isPlayerDisconnected : ClientId -> Storage -> Bool
isPlayerDisconnected clientId storage =
    Dict.member clientId storage

removeExpiredEntries : Posix -> Time.Posix -> Storage -> Storage
removeExpiredEntries currentTime timeout storage =
    Dict.filter (\_ playerInfo -> Time.diff currentTime playerInfo.disconnectedAt Time.second < timeout) storage
