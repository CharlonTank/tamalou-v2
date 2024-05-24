module Evergreen.V54.InternalAnim.Time exposing (..)

import Evergreen.V54.InternalAnim.Duration
import Evergreen.V54.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V54.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V54.InternalAnim.Duration.Duration
