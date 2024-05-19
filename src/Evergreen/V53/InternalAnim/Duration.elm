module Evergreen.V53.InternalAnim.Duration exposing (..)

import Evergreen.V53.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V53.InternalAnim.Quantity.Quantity Float Seconds
