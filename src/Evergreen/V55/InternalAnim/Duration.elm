module Evergreen.V55.InternalAnim.Duration exposing (..)

import Evergreen.V55.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V55.InternalAnim.Quantity.Quantity Float Seconds
