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
            newX =
                x + len * cos angle

            newY =
                y + len * sin angle
        in
        group
            [ openPolygon [ ( newX, newY ), ( x, y ), ( newX, newY ) ] |> color
            , branch newX newY (len * 0.75) (angle - 1)
            , branch newX newY (len * 0.75) (angle + 1)
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
