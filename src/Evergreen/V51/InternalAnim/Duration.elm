module Evergreen.V51.InternalAnim.Duration exposing (..)

import Evergreen.V51.InternalAnim.Quantity


type Seconds
    = Seconds


type alias Duration =
    Evergreen.V51.InternalAnim.Quantity.Quantity Float Seconds
