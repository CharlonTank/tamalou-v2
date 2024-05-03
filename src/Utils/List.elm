module Utils.List exposing (..)


findAndRemove : (a -> Bool) -> List a -> ( Maybe a, List a )
findAndRemove predicate list =
    let
        ( found, rest ) =
            List.partition predicate list
    in
    case found of
        [ x ] ->
            ( Just x, rest )

        _ ->
            ( Nothing, list )
