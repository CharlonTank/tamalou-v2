module Utils.Error exposing (..)

import Types exposing (FrontendMsg)
import Ui exposing (Element, shrink, text, width)
import Ui.Prose as Prose


displayError : String -> Element FrontendMsg
displayError error =
    Prose.paragraph [ width shrink ] [ text error ]
