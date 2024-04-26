module Utils.Element exposing (attributeNone)

import Html.Attributes as HA
import Types exposing (FrontendMsg)
import Ui


attributeNone : Ui.Attribute FrontendMsg
attributeNone =
    Ui.htmlAttribute <| HA.class ""
