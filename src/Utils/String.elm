module Utils.String exposing (fromBool)


fromBool : Bool -> String
fromBool b =
    if b then
        "True"

    else
        "False"
