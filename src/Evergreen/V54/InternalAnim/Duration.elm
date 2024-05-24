module Evergreen.V54.InternalAnim.Duration exposing (..)

import Evergreen.V54.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V54.InternalAnim.Quantity.Quantity Float Seconds
