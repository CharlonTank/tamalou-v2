module Utils.Element exposing (attributeNone)

import Html.Attributes as HA
import Types exposing (FrontendMsg)
import Ui
import Ui.Anim
import Ui.Layout
import Ui.Prose


attributeNone : Ui.Attribute FrontendMsg
attributeNone =
    Ui.htmlAttribute <| HA.class ""
