module Counter exposing (..)


type Counter
    = Five
    | Four
    | Three
    | Two
    | One


decrement : Counter -> Maybe Counter
decrement counter =
    case counter of
        Five ->
            Just Four

        Four ->
            Just Three

        Three ->
            Just Two

        Two ->
            Just One

        One ->
            Nothing
