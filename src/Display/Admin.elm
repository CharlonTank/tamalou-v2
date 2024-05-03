module Display.Admin exposing (..)

import Types exposing (FrontendModel, FrontendMsg)
import Ui exposing (Element, column, fill, height, spacing)
import Utils.Error exposing (displayError)


displayAdmin : FrontendModel -> Element FrontendMsg
displayAdmin model =
    List.map displayError model.errors
        |> column [ height fill, spacing 16 ]
