module Palette.Anim exposing (..)

import Element exposing (Element)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Anim
import Simple.Animation.Property as Anim


rotateAnim : Animation
rotateAnim =
    Animation.steps
        { startAt = []
        , options = [ Animation.loop ]
        }
        [ Animation.step 2000 [ Anim.rotate 720, Anim.opacity 0.8 ]
        , Animation.step 2000 [ Anim.rotate 1440, Anim.opacity 1 ]
        ]


fadeAnim : Animation
fadeAnim =
    Animation.steps
        { startAt = []
        , options = [ Animation.loop ]
        }
        [ Animation.step 2000 [ Anim.opacity 0.2 ]
        , Animation.step 1500 [ Anim.opacity 1 ]
        ]


animatedUi : (List (Element.Attribute msg) -> children -> Element msg) -> Animation -> List (Element.Attribute msg) -> children -> Element msg
animatedUi =
    Anim.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


el : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
el =
    animatedUi Element.el


row : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
row =
    animatedUi Element.row


column : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
column =
    animatedUi Element.column


customTransformOrigin : String -> Anim.Property
customTransformOrigin origin =
    Anim.property "transform-origin" origin
