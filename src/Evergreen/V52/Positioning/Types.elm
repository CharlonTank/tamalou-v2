module Evergreen.V52.Positioning.Types exposing (..)

import Evergreen.V52.Ui


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V52.Ui.Angle
    }
