module Router exposing (..)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, string, top)


type Route
    = Home
    | Admin
    | Game String


routerParser : Parser (Route -> a) a
routerParser =
    oneOf
        [ map Home top
        , map Admin (s "admin")
        , map Game string
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse routerParser url of
        Just route ->
            route

        Nothing ->
            Home
