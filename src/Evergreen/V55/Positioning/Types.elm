module Evergreen.V55.Positioning.Types exposing (..)

import Evergreen.V55.Ui


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V55.Ui.Angle
    }
