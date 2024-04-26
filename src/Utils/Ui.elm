module Utils.Ui exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)

{-| -}


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
