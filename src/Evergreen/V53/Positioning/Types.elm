module Evergreen.V53.Positioning.Types exposing (..)

import Evergreen.V53.Ui


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V53.Ui.Angle
    }
