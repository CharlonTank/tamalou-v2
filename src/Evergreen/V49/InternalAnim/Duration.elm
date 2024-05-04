module Evergreen.V49.InternalAnim.Duration exposing (..)

import Evergreen.V49.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V49.InternalAnim.Quantity.Quantity Float Seconds
