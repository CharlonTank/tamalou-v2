module Evergreen.V49.Positioning.Types exposing (..)

import Evergreen.V49.Ui


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V49.Ui.Angle
    }
