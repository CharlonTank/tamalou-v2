module Palette.Anim exposing (animatedUi, column, customTransformOrigin, el, fadeAnim, minimalistPhoneWithHint, rotateAnim, row)

import Simple.Animation as A exposing (Animation)
import Simple.Animation.Animated as Anim
import Simple.Animation.Property as AP
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Ui
import Ui.Anim
import Ui.Layout
import Ui.Prose


rotateAnim : Animation
rotateAnim =
    A.steps
        { options = [ A.loop ]
        , startAt = []
        }
        [ A.step 2000 [ AP.rotate 720, AP.opacity 0.8 ]
        , A.step 2000 [ AP.rotate 1440, AP.opacity 1 ]
        ]


fadeAnim : Animation
fadeAnim =
    A.steps
        { options = [ A.loop ]
        , startAt = []
        }
        [ A.step 2000 [ AP.opacity 0.2 ]
        , A.step 1500 [ AP.opacity 1 ]
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


customTransformOrigin : String -> AP.Property
customTransformOrigin origin =
    AP.property "transform-origin" origin


phoneRotateAnimation : Animation
phoneRotateAnimation =
    A.steps
        { options = [ A.loop ]
        , startAt = [ AP.rotate 0, customTransformOrigin "center" ]
        }
        [ A.step 500 [ AP.rotate 90, customTransformOrigin "center" ]
        , A.wait 300
        , A.step 200 [ AP.rotate 0, customTransformOrigin "center" ]
        , A.wait 300
        ]


minimalistPhoneWithHint : Svg msg
minimalistPhoneWithHint =
    Svg.svg [ SvgA.viewBox "0 0 100 100" ]
        [ Anim.svg { class = SvgA.class } Svg.g phoneRotateAnimation [] [ phoneSvg ]
        ]


phoneSvg : Svg msg
phoneSvg =
    Svg.g
        [ SvgA.width "50"
        , SvgA.height "50"
        ]
        [ centeredRect [ SvgA.fill "black" ] 40 30 3
        , centeredRect [ SvgA.fill "grey" ] 41 31 2
        , Svg.circle [ SvgA.cx "50", SvgA.cy "31.6", SvgA.r "0.4", SvgA.fill "black" ] []
        , Svg.rect [ SvgA.x "39.8", SvgA.y "35", SvgA.width "0.5", SvgA.height "3", SvgA.rx "0.5", SvgA.ry "0.5", SvgA.fill "grey" ] []
        ]


centeredRect : List (Svg.Attribute msg) -> Int -> Int -> Int -> Svg msg
centeredRect attributes x y radius =
    let
        rectHeight : String
        rectHeight =
            (100 - y * 2) |> String.fromInt

        rectWidth : String
        rectWidth =
            (100 - x * 2) |> String.fromInt

        xInt : String
        xInt =
            String.fromInt x

        yInt : String
        yInt =
            String.fromInt y
    in
    Svg.rect
        (attributes
            ++ [ SvgA.x xInt
               , SvgA.width rectWidth
               , SvgA.y yInt
               , SvgA.height rectHeight
               , SvgA.rx (String.fromInt radius)
               , SvgA.ry (String.fromInt radius)
               ]
        )
        []
