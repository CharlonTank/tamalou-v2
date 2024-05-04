module Evergreen.V49.InternalAnim.Time exposing (..)

import Evergreen.V49.InternalAnim.Duration
import Evergreen.V49.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V49.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V49.InternalAnim.Duration.Duration
