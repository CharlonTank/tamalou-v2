module Player exposing (..)

import Card exposing (Card, FCard, Power, toFCard)
import Counter exposing (Counter)
import Lamdera exposing (ClientId, SessionId)


type alias BPlayer =
    { name : String
    , tableHand : List Card
    , clientId : ClientId
    , sessionId : SessionId
    , ready : Bool
    }


type BPlayerToPlayStatus
    = BWaitingPlayerAction (Maybe Power)
    | BPlayerHasDrawn Card
    | BPlayerHasDiscard Power
    | BPlayerLookACard LookACardStatus
    | BPlayerSwitch2Cards Switch2CardsStatus
    | BPlayerDisplayTamalouFailure Counter


type alias FPlayer =
    { name : String
    , tableHand : List FCard
    , clientId : ClientId
    , sessionId : SessionId
    , ready : Bool
    , score : Maybe Int
    }


type FPlayerToPlayStatus
    = FWaitingPlayerAction (Maybe Power)
    | FPlayerHasDraw FCard
    | FPlayerHasDiscard Power
    | FPlayerLookACard LookACardStatus
    | FPlayerSwitch2Cards Switch2CardsStatus


type LookACardStatus
    = ChooseCardToLook
    | LookingACard Int Counter


type Switch2CardsStatus
    = ChooseOwnCardToSwitch
    | OwnCardChosen Int
    | OpponentCardChosen Int { sessionId : SessionId, index : Int } Counter


toBPlayer : FPlayer -> BPlayer
toBPlayer fPlayer =
    { name = fPlayer.name
    , tableHand = []
    , clientId = fPlayer.clientId
    , sessionId = fPlayer.sessionId
    , ready = fPlayer.ready
    }


toFPlayer : Bool -> BPlayer -> FPlayer
toFPlayer sendScore bPlayer =
    { name = bPlayer.name
    , tableHand = List.map toFCard bPlayer.tableHand
    , clientId = bPlayer.clientId
    , sessionId = bPlayer.sessionId
    , ready = bPlayer.ready
    , score =
        if sendScore then
            Just <| Card.tableHandScore bPlayer.tableHand

        else
            Nothing
    }


showAllCardsOfAllPlayers : List ( BPlayer, Int ) -> List ( BPlayer, Int )
showAllCardsOfAllPlayers players =
    List.map
        (\( { tableHand } as player, r ) ->
            ( { player | tableHand = List.map (\card -> { card | show = True }) tableHand }, r )
        )
        players


showTamalouOwnerCards : SessionId -> List BPlayer -> List BPlayer
showTamalouOwnerCards tamalouOwnerSessionId players =
    List.map
        (\player ->
            if player.sessionId == tamalouOwnerSessionId then
                { player | tableHand = List.map (\card -> { card | show = True }) player.tableHand }

            else
                player
        )
        players


stopDisplayCards : Maybe SessionId -> List BPlayer -> List BPlayer
stopDisplayCards tamalouOwnerSessionId players =
    case tamalouOwnerSessionId of
        Just ownerId ->
            List.map
                (\player ->
                    if player.sessionId == ownerId then
                        player

                    else
                        { player | tableHand = List.map (\card -> { card | show = False }) player.tableHand }
                )
                players

        Nothing ->
            List.map
                (\player ->
                    { player | tableHand = List.map (\card -> { card | show = False }) player.tableHand }
                )
                players


emptyBPlayer : SessionId -> ClientId -> BPlayer
emptyBPlayer sessionId clientId =
    { name = "Anonymous"
    , tableHand = []
    , clientId = clientId
    , sessionId = sessionId
    , ready = False
    }
