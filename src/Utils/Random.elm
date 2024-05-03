module Utils.Random exposing (..)

import Card exposing (Card)
import Game exposing (BDrawPile, BGame, BGameStatus(..), DiscardPile)
import List.Extra
import Random


generateRandomFunnyName : Random.Seed -> List String -> ( String, Random.Seed )
generateRandomFunnyName seed alreadyNames =
    let
        filteredNames : List String
        filteredNames =
            List.filter (\name -> not (List.member name alreadyNames)) listOfFunnyPlaceHolderNames
    in
    Random.step (Random.int 0 (List.length filteredNames - 1)) seed
        |> (\( n, newSeed ) -> ( List.Extra.getAt n filteredNames |> Maybe.withDefault "Anonymous", newSeed ))


listOfFunnyPlaceHolderNames : List String
listOfFunnyPlaceHolderNames =
    [ "Francis Bacon"
    , "Benedict Egg"
    , "Taco Belle"
    , "Maxi Mom"
    , "Mini Mom"
    , "Sal A. Mander"
    , "Holly Wood"
    , "Alain Delonion"
    , "Jean DuJardinage"
    , "Samuel L. Jackfruit"
    , "Tom Ato"
    , "Framboise A. LaCreme"
    , "Jean-Claude Sans Dame"
    , "Tom C"
    , "Leonardo DiCarpaccio"
    , "Idris Elbarmesan"
    , "Marilyn Monrouleau"
    , "Jean-Paul Tartre"
    , "Angelina Jolie Haricot"
    , "Albert Einchampignon"
    ]


shuffleWithSeed : Random.Seed -> List a -> ( List a, Random.Seed )
shuffleWithSeed initialSeed list =
    let
        ( taggedList, finalSeed ) =
            list
                |> List.foldl
                    (\item ( acc, currentSeed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step (Random.int Random.minInt Random.maxInt) currentSeed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], initialSeed )

        shuffledList : List a
        shuffledList =
            taggedList
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
    in
    ( shuffledList, finalSeed )


drawCardFromDrawPile : BGame -> ( Maybe Card, ( BDrawPile, DiscardPile, Random.Seed ) )
drawCardFromDrawPile game =
    case game.status of
        BGameInProgress _ drawPile discardPile _ _ _ _ ->
            case ( drawPile, discardPile ) of
                ( [], [] ) ->
                    ( Nothing, ( [], [], game.seed ) )

                ( [], [ singleCard ] ) ->
                    ( Just singleCard, ( [], [], game.seed ) )

                ( [], firstCard :: restOfDiscardPile ) ->
                    let
                        ( shuffledRestDiscardPile, newSeed ) =
                            shuffleWithSeed game.seed restOfDiscardPile
                    in
                    case List.Extra.uncons shuffledRestDiscardPile of
                        Just ( cardDrew, newDrawPile ) ->
                            ( Just cardDrew, ( newDrawPile, [ firstCard ], newSeed ) )

                        Nothing ->
                            ( Nothing, ( [], [ firstCard ], newSeed ) )

                ( [ singleCard ], [] ) ->
                    ( Just singleCard, ( [], [], game.seed ) )

                ( [ singleCard ], firstCard :: restOfDiscardPile ) ->
                    let
                        ( shuffledRestDiscardPile, newSeed ) =
                            shuffleWithSeed game.seed restOfDiscardPile
                    in
                    ( Just singleCard, ( shuffledRestDiscardPile, [ firstCard ], newSeed ) )

                ( firstCard :: restOfDrawPile, _ ) ->
                    ( Just firstCard, ( restOfDrawPile, discardPile, game.seed ) )

        _ ->
            ( Nothing, ( [], [], game.seed ) )
