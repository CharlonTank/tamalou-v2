module Evergreen.V53.InternalAnim.Time exposing (..)

import Evergreen.V53.InternalAnim.Duration
import Evergreen.V53.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V53.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V53.InternalAnim.Duration.Duration
