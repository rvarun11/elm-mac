module Plants.FractalTrees exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)


color : Stencil -> Shape msg
color =
    outlined (solid 1) darkGreen


trunk : Float -> Float -> Float -> Float -> Shape msg
trunk x1 y1 x2 y2 =
    line ( x1, y1 ) ( x2, y2 ) |> outlined (solid 1) darkBrown


branch : Float -> Float -> Float -> Float -> Shape msg
branch x y len angle =
    if len <= 1 then
        group []

    else
        let
            x1 =
                x + len * cos angle

            y1 =
                y + len * sin angle

            x2 =
                x + len * cos angle

            y2 =
                y + len * sin angle
        in
        group
            [ openPolygon [ ( x2, y2 ), ( x, y ), ( x1, y1 ) ] |> color
            , branch x1 y1 (len * 0.75) (angle - 1)
            , branch x2 y2 (len * 0.75) (angle + 1)
            ]



-- initialize branch


view : Collage msg
view =
    collage 300
        300
        [ graphPaper 10
        , trunk 0 0 0 -120
        , branch 0 0 30 (pi / 4)
        , branch 0 0 30 (3 * pi / 4)
        ]


main =
    graphicsApp { view = view }
