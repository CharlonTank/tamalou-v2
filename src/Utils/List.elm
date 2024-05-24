module Utils.List exposing (..)

import List.Extra as List


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


findAndRearrange : (a -> Bool) -> List a -> ( Maybe a, List a )
findAndRearrange predicate list =
    case List.findIndex predicate list of
        Just index ->
            let
                after : List a
                after =
                    List.drop 1 foundAndAfter

                ( before, foundAndAfter ) =
                    List.splitAt index list

                found : Maybe a
                found =
                    List.head foundAndAfter
            in
            ( found, after ++ before )

        Nothing ->
            ( Nothing, list )
