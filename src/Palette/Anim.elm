module Palette.Anim exposing (animatedUi, column, customTransformOrigin, el, fadeAnim, rotateAnim, row)

import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Anim
import Simple.Animation.Property as Anim
import Ui
import Ui.Anim
import Ui.Layout
import Ui.Prose


rotateAnim : Animation
rotateAnim =
    Animation.steps
        { options = [ Animation.loop ]
        , startAt = []
        }
        [ Animation.step 2000 [ Anim.rotate 720, Anim.opacity 0.8 ]
        , Animation.step 2000 [ Anim.rotate 1440, Anim.opacity 1 ]
        ]


fadeAnim : Animation
fadeAnim =
    Animation.steps
        { options = [ Animation.loop ]
        , startAt = []
        }
        [ Animation.step 2000 [ Anim.opacity 0.2 ]
        , Animation.step 1500 [ Anim.opacity 1 ]
        ]


animatedUi : (List (Ui.Attribute msg) -> children -> Ui.Element msg) -> Animation -> List (Ui.Attribute msg) -> children -> Ui.Element msg
animatedUi =
    Anim.ui
        { behindContent = Ui.behindContent
        , html = Ui.html
        , htmlAttribute = Ui.htmlAttribute
        }


el : Animation -> List (Ui.Attribute msg) -> Ui.Element msg -> Ui.Element msg
el =
    animatedUi Ui.el


row : Animation -> List (Ui.Attribute msg) -> List (Ui.Element msg) -> Ui.Element msg
row =
    animatedUi Ui.row


column : Animation -> List (Ui.Attribute msg) -> List (Ui.Element msg) -> Ui.Element msg
column =
    animatedUi Ui.column


customTransformOrigin : String -> Anim.Property
customTransformOrigin origin =
    Anim.property "transform-origin" origin
