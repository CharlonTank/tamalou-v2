module Evergreen.V40.InternalAnim.Duration exposing (..)

import Evergreen.V40.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V40.InternalAnim.Quantity.Quantity Float Seconds
