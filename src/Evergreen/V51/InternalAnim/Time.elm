module Evergreen.V51.InternalAnim.Time exposing (..)

import Evergreen.V51.InternalAnim.Duration
import Evergreen.V51.InternalAnim.Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Evergreen.V51.InternalAnim.Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Evergreen.V51.InternalAnim.Duration.Duration
