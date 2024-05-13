module Evergreen.V52.InternalAnim.Duration exposing (..)

import Evergreen.V52.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V52.InternalAnim.Quantity.Quantity Float Seconds
