module Evergreen.V52.InternalAnim.Time exposing (..)

import Evergreen.V52.InternalAnim.Duration
import Evergreen.V52.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V52.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V52.InternalAnim.Duration.Duration
