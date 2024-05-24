module Evergreen.V54.Positioning.Types exposing (..)

import Evergreen.V54.Ui


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V54.Ui.Angle
    }
