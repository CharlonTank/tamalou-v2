module Utils.Ui exposing (Device, DeviceClass(..), Orientation(..), actionBorder, bigShadow, classifyDevice)

{-| -}

-- import Types exposing (FMsg)

import Ui exposing (..)
import Ui.Shadow as Shadow


type alias Device =
    { class : DeviceClass
    , orientation : Orientation
    }


{-| -}
type DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


{-| -}
type Orientation
    = Portrait
    | Landscape


actionBorder : Color -> List (Attribute msg)
actionBorder color =
    [ rounded 8
    , background color
    , paddingXY 4 4
    , minimalistShadow
    ]


bigShadow : Ui.Color -> Ui.Attribute msg
bigShadow color =
    Shadow.shadows
        [ { blur = 8
          , color = color
          , size = 4
          , x = 0
          , y = 0
          }
        ]


{-| Takes in a Window.Size and returns a device profile which can be used for responsiveness.

If you have more detailed concerns around responsiveness, it probably makes sense to copy this function into your codebase and modify as needed.

-}
classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    { class =
        let
            shortSide : Int
            shortSide =
                min window.width window.height
        in
        if shortSide < 600 then
            Phone

        else
            let
                longSide : Int
                longSide =
                    max window.width window.height
            in
            if longSide <= 1200 then
                Tablet

            else if longSide > 1200 && longSide <= 1920 then
                Desktop

            else
                BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }


minimalistShadow : Ui.Attribute msg
minimalistShadow =
    Shadow.shadows
        [ { blur = 2
          , color = rgba 0 0 0 0.2
          , size = 1
          , x = 0
          , y = 0
          }
        ]
