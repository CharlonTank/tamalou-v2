module Evergreen.V40.Utils.Ui exposing (..)


type DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


type Orientation
    = Portrait
    | Landscape


type alias Device =
    { class : DeviceClass
    , orientation : Orientation
    }
