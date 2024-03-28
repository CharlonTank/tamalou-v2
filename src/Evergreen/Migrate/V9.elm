module Evergreen.Migrate.V9 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Basics
import Evergreen.V8.Card
import Evergreen.V8.Types
import Evergreen.V9.Card
import Evergreen.V9.Types
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V8.Types.FrontendModel -> ModelMigration Evergreen.V9.Types.FrontendModel Evergreen.V9.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V8.Types.BackendModel -> ModelMigration Evergreen.V9.Types.BackendModel Evergreen.V9.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V8.Types.FrontendMsg -> MsgMigration Evergreen.V9.Types.FrontendMsg Evergreen.V9.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V8.Types.ToBackend -> MsgMigration Evergreen.V9.Types.ToBackend Evergreen.V9.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V8.Types.BackendMsg -> MsgMigration Evergreen.V9.Types.BackendMsg Evergreen.V9.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V8.Types.ToFrontend -> MsgMigration Evergreen.V9.Types.ToFrontend Evergreen.V9.Types.FrontendMsg
toFrontend old =
    MsgUnchanged



-- migrate_Types_BackendModel : Evergreen.V8.Types.BackendModel -> Evergreen.V9.Types.BackendModel
-- migrate_Types_BackendModel old =
--     { games = old.games |> List.map migrate_Types_BGame
--     }
-- migrate_Types_FrontendModel : Evergreen.V8.Types.FrontendModel -> Evergreen.V9.Types.FrontendModel
-- migrate_Types_FrontendModel old =
--     { key = old.key
--     , device = old.device
--     , gameFrontend = old.gameFrontend |> migrate_Types_FGame
--     , clientId = old.clientId
--     , sessionId = old.sessionId
--     , urlPath = old.urlPath
--     }


migrate_Card_Card : Evergreen.V8.Card.Card -> Evergreen.V9.Card.Card
migrate_Card_Card old =
    { suit = old.suit |> migrate_Card_Suit
    , rank = old.rank |> migrate_Card_Rank
    , show = old.show
    }


migrate_Card_FCard : Evergreen.V8.Card.FCard -> Evergreen.V9.Card.FCard
migrate_Card_FCard old =
    case old of
        Evergreen.V8.Card.FaceUp p0 ->
            Evergreen.V9.Card.FaceUp (p0 |> migrate_Card_Card)

        Evergreen.V8.Card.FaceDown ->
            Evergreen.V9.Card.FaceDown


migrate_Card_Rank : Evergreen.V8.Card.Rank -> Evergreen.V9.Card.Rank
migrate_Card_Rank old =
    case old of
        Evergreen.V8.Card.Ace ->
            Evergreen.V9.Card.Ace

        Evergreen.V8.Card.Two ->
            Evergreen.V9.Card.Two

        Evergreen.V8.Card.Three ->
            Evergreen.V9.Card.Three

        Evergreen.V8.Card.Four ->
            Evergreen.V9.Card.Four

        Evergreen.V8.Card.Five ->
            Evergreen.V9.Card.Five

        Evergreen.V8.Card.Six ->
            Evergreen.V9.Card.Six

        Evergreen.V8.Card.Seven ->
            Evergreen.V9.Card.Seven

        Evergreen.V8.Card.Eight ->
            Evergreen.V9.Card.Eight

        Evergreen.V8.Card.Nine ->
            Evergreen.V9.Card.Nine

        Evergreen.V8.Card.Ten ->
            Evergreen.V9.Card.Ten

        Evergreen.V8.Card.Jack ->
            Evergreen.V9.Card.Jack

        Evergreen.V8.Card.Queen ->
            Evergreen.V9.Card.Queen

        Evergreen.V8.Card.King ->
            Evergreen.V9.Card.King


migrate_Card_Suit : Evergreen.V8.Card.Suit -> Evergreen.V9.Card.Suit
migrate_Card_Suit old =
    case old of
        Evergreen.V8.Card.Clubs ->
            Evergreen.V9.Card.Clubs

        Evergreen.V8.Card.Diamonds ->
            Evergreen.V9.Card.Diamonds

        Evergreen.V8.Card.Hearts ->
            Evergreen.V9.Card.Hearts

        Evergreen.V8.Card.Spades ->
            Evergreen.V9.Card.Spades


migrate_Types_BDrawPile : Evergreen.V8.Types.DiscardPile -> Evergreen.V9.Types.BDrawPile
migrate_Types_BDrawPile old =
    old |> List.map migrate_Card_Card



-- migrate_Types_BGame : Evergreen.V8.Types.BGame -> Evergreen.V9.Types.BGame
-- migrate_Types_BGame old =
--     { urlPath = old.urlPath
--     , status = old.status |> migrate_Types_BGameStatus
--     , seed = old.seed
--     }
-- migrate_Types_BGameInProgressStatus : Basics.BGameInProgressStatus -> Evergreen.V9.Types.BGameInProgressStatus
-- migrate_Types_BGameInProgressStatus old =
--     case old of
--         Basics.True ->
--             (Unimplemented
--              {- `True` was removed or renamed in V9 so I couldn't figure out how to migrate it.
--                 I need you to decide what happens to this Basics.True value in a migration.
--                 See https://lamdera.com/tips/modified-custom-type for more info.
--              -}
--             )
--         Basics.False ->
--             (Unimplemented
--              {- `False` was removed or renamed in V9 so I couldn't figure out how to migrate it.
--                 I need you to decide what happens to this Basics.False value in a migration.
--                 See https://lamdera.com/tips/modified-custom-type for more info.
--              -}
--             )
--         notices ->
--             {- @NOTICE `BTimerRunning Int` was added in V9.
--                This is just a reminder in case migrating some subset of the old data to this new value was important.
--                See https://lamdera.com/tips/modified-custom-type for more info.
--             -}
--             {- @NOTICE `BPlayerToPlay lamdera/core:Lamdera.SessionId Evergreen.V9.Types.BPlayerToPlayStatus` was added in V9.
--                This is just a reminder in case migrating some subset of the old data to this new value was important.
--                See https://lamdera.com/tips/modified-custom-type for more info.
--             -}
--             (Unimplemented {- New constructors were added. I need you to resolve the above notices and then remove this case. -})
-- migrate_Types_BGameStatus : Evergreen.V8.Types.BGameStatus -> Evergreen.V9.Types.BGameStatus
-- migrate_Types_BGameStatus old =
--     case old of
--         Evergreen.V8.Types.BWaitingForPlayers p0 ->
--             Evergreen.V9.Types.BWaitingForPlayers (p0 |> List.map migrate_Types_BPlayer)
--         Evergreen.V8.Types.BGameInProgress p0 p1 p2 p3 p4 ->
--             Evergreen.V9.Types.BGameInProgress (p0 |> (Unimplemented {- Type changed from `Evergreen.V8.Types.BDrawPile` to `Maybe (lamdera/core:Lamdera.SessionId)`. I need you to write this migration. -}))
--                 (p1 |> migrate_Types_BDrawPile)
--                 (p2 |> (Unimplemented {- Type changed from `List (Evergreen.V8.Types.BPlayer)` to `Evergreen.V9.Types.DiscardPile`. I need you to write this migration. -}))
--                 (p3 |> (Unimplemented {- Type changed from `Evergreen.V8.Types.BGameInProgressStatus` to `List (Evergreen.V9.Types.BPlayer)`. I need you to write this migration. -}))
--                 (p4 |> migrate_Types_BGameInProgressStatus)
--                 (Unimplemented {- This new variant needs to be initialised -})
--         Evergreen.V8.Types.BGameEnded p0 ->
--             Evergreen.V9.Types.BGameEnded p0


migrate_Types_BPlayer : Evergreen.V8.Types.BPlayer -> Evergreen.V9.Types.BPlayer
migrate_Types_BPlayer old =
    { name = old.name
    , tableHand = old.tableHand |> List.map migrate_Card_Card
    , clientId = old.clientId
    , sessionId = old.sessionId
    }



-- migrate_Types_FDrawPile : Evergreen.V8.Types.DiscardPile -> Evergreen.V9.Types.FDrawPile
-- migrate_Types_FDrawPile old =
--     old |> List.map (Unimplemented {- Type changed from `Evergreen.V8.Card.Card` to `Evergreen.V9.Card.FCard`. I need you to write this migration. -})
-- migrate_Types_FGame : Evergreen.V8.Types.FGame -> Evergreen.V9.Types.FGame
-- migrate_Types_FGame old =
--     case old of
--         Evergreen.V8.Types.FWaitingForPlayers p0 ->
--             Evergreen.V9.Types.FWaitingForPlayers (p0 |> List.map migrate_Types_FPlayer)
--         Evergreen.V8.Types.FGameInProgress p0 p1 p2 p3 p4 ->
--             Evergreen.V9.Types.FGameInProgress (p0 |> (Unimplemented {- Type changed from `Evergreen.V8.Types.FTableHand` to `Maybe (Evergreen.V9.Types.TamalouOwner)`. I need you to write this migration. -}))
--                 (p1 |> migrate_Types_FTableHand)
--                 (p2 |> migrate_Types_FDrawPile)
--                 (p3 |> (Unimplemented {- Type changed from `List (Evergreen.V8.Types.FPlayer)` to `Evergreen.V9.Types.DiscardPile`. I need you to write this migration. -}))
--                 (p4 |> (Unimplemented {- Type changed from `Evergreen.V8.Types.FGameInProgressStatus` to `List (Evergreen.V9.Types.FPlayer)`. I need you to write this migration. -}))
--                 (Unimplemented {- This new variant needs to be initialised -})
--         Evergreen.V8.Types.FGameEnded p0 ->
--             Evergreen.V9.Types.FGameEnded p0
--         Evergreen.V8.Types.FGameAlreadyStartedWithoutYou ->
--             Evergreen.V9.Types.FGameAlreadyStartedWithoutYou


migrate_Types_FPlayer : Evergreen.V8.Types.FPlayer -> Evergreen.V9.Types.FPlayer
migrate_Types_FPlayer old =
    { name = old.name
    , tableHand = old.tableHand |> List.map migrate_Card_FCard
    , clientId = old.clientId
    , sessionId = old.sessionId
    }


migrate_Types_FTableHand : Evergreen.V8.Types.FDrawPile -> Evergreen.V9.Types.FTableHand
migrate_Types_FTableHand old =
    old |> List.map migrate_Card_FCard



-- migrate_Types_ToBackend : Evergreen.V8.Types.ToBackend -> Evergreen.V9.Types.ToBackend
-- migrate_Types_ToBackend old =
--     case old of
--         Evergreen.V8.Types.NoOpToBackend ->
--             Evergreen.V9.Types.NoOpToBackend
--         Evergreen.V8.Types.ToBackendActionFromGame p0 p1 ->
--             Evergreen.V9.Types.ToBackendActionFromGame p0 (p1 |> migrate_Types_ToBackendActionFromGame)
-- migrate_Types_ToBackendActionFromGame : Evergreen.V8.Types.ToBackendActionFromGame -> Evergreen.V9.Types.ToBackendActionFromGame
-- migrate_Types_ToBackendActionFromGame old =
--     case old of
--         Evergreen.V8.Types.ConnectToBackend ->
--             Evergreen.V9.Types.ConnectToBackend
--         Evergreen.V8.Types.DrawCardFromDrawPileToBackend ->
--             Evergreen.V9.Types.DrawCardFromDrawPileToBackend
--         Evergreen.V8.Types.DiscardCardInHandToBackend ->
--             Evergreen.V9.Types.DiscardCardInHandToBackend
--         Evergreen.V8.Types.DrawCardFromDiscardPileToBackend ->
--             Evergreen.V9.Types.DrawCardFromDiscardPileToBackend
--         Evergreen.V8.Types.ReplaceCardInTableHandToBackend p0 ->
--             Evergreen.V9.Types.ReplaceCardInTableHandToBackend p0
--         Evergreen.V8.Types.DoubleCardInTableHandToBackend p0 ->
--             Evergreen.V9.Types.DoubleCardInTableHandToBackend p0
--         notices ->
--             {- @NOTICE `TamalouToBackend` was added in V9.
--                This is just a reminder in case migrating some subset of the old data to this new value was important.
--                See https://lamdera.com/tips/modified-custom-type for more info.
--             -}
--             (Unimplemented {- New constructors were added. I need you to resolve the above notices and then remove this case. -})
-- migrate_Types_ToFrontend : Evergreen.V8.Types.ToFrontend -> Evergreen.V9.Types.ToFrontend
-- migrate_Types_ToFrontend old =
--     case old of
--         Evergreen.V8.Types.NoOpToFrontend ->
--             Evergreen.V9.Types.NoOpToFrontend
--         Evergreen.V8.Types.UpdateGame p0 ->
--             Evergreen.V9.Types.UpdateGame (p0 |> migrate_Types_FGame)
--         Evergreen.V8.Types.GotSessionIdAndClientId p0 p1 ->
--             Evergreen.V9.Types.GotSessionIdAndClientId p0 p1
