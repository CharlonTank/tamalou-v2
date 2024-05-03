module Positioning.Helpers exposing (findCardPosition, fixReverseSpinningEffectRotation, fixSpinningEffectRotation, offSet, scrollToBottom, wantedSpinningRotationValue)

import Animator.Timeline exposing (Timeline)
import Browser.Dom
import Internal.Style2 exposing (toRadians)
import Lamdera exposing (SessionId)
import List.Extra
import Positioning.Types exposing (GBPosition)
import Task
import Types exposing (FrontendMsg(..), OpponentsDisposition, PositionedPlayer)
import Ui exposing (Position)


findCardPosition : SessionId -> Int -> OpponentsDisposition -> Maybe (Timeline GBPosition)
findCardPosition sessionId index { leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer } =
    let
        findInPlayer : PositionedPlayer -> Maybe (Timeline GBPosition)
        findInPlayer positionedPlayer =
            if sessionId == positionedPlayer.player.sessionId then
                positionedPlayer.positionedTableHand
                    |> List.Extra.getAt index
                    |> Maybe.map Tuple.second

            else
                Nothing
    in
    [ leftPlayer, topLeftPlayer, topRightPlayer, rightPlayer ]
        |> List.filterMap (Maybe.andThen findInPlayer)
        |> List.head


fixReverseSpinningEffectRotation : GBPosition -> GBPosition
fixReverseSpinningEffectRotation gbPosition =
    { gbPosition
        | rotation = Ui.radians <| toRadians gbPosition.rotation - wantedSpinningRotationValue
    }


fixSpinningEffectRotation : GBPosition -> GBPosition
fixSpinningEffectRotation gbPosition =
    { gbPosition
        | rotation = Ui.radians <| toRadians gbPosition.rotation + wantedSpinningRotationValue
    }


offSet : Position
offSet =
    { x = 0
    , y = 0
    , z = 0
    }


scrollToBottom : String -> Cmd FrontendMsg
scrollToBottom elementId =
    Browser.Dom.getViewportOf elementId
        |> Task.andThen
            (\viewport ->
                Browser.Dom.setViewportOf elementId 0 viewport.scene.height
            )
        |> Task.attempt (always NoOpFrontendMsg)


wantedSpinningRotationValue : Float
wantedSpinningRotationValue =
    2 * pi
