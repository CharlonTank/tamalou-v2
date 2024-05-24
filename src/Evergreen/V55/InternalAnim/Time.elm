module Evergreen.V55.InternalAnim.Time exposing (..)

import Evergreen.V55.InternalAnim.Duration
import Evergreen.V55.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V55.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V55.InternalAnim.Duration.Duration
