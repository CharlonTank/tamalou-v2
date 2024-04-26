module Evergreen.V40.InternalAnim.Time exposing (..)

import Evergreen.V40.InternalAnim.Duration
import Evergreen.V40.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V40.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V40.InternalAnim.Duration.Duration
