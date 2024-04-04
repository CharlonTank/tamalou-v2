module Utils.Element exposing (attributeNone)

import Element exposing (Attribute, htmlAttribute)
import Html.Attributes as HA
import Types exposing (FrontendMsg)


attributeNone : Attribute FrontendMsg
attributeNone =
    htmlAttribute <| HA.class ""
