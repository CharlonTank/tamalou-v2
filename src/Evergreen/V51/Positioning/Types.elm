module Evergreen.V51.Positioning.Types exposing (..)

import Evergreen.V51.Ui


type alias GBPosition =
    { x : Float
    , y : Float
    , width_ : Float
    , height_ : Float
    , rotation : Evergreen.V51.Ui.Angle
    }
