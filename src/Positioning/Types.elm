module Positioning.Types exposing (..)

import Ui


type VisibleAngle
    = AngleZero
    | AnglePiOverTwo
    | AnglePi
    | AngleThreePiOverTwo


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Ui.Angle
    }


type OpponentDisposition
    = LeftPlayer
    | TopLeftPlayer
    | TopRightPlayer
    | RightPlayer
