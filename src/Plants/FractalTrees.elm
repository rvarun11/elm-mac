module Plants.FractalTrees exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)


colr : Stencil -> Shape msg
colr =
    outlined (solid 1) black


branch : Float -> Float -> Float -> Shape msg
branch x y angle =
    let
        y2 =
            y - 10

        x2 =
            x + y2 * sin (degrees 0)
    in
    group
        [ line ( 0, 0 ) ( x, y ) |> colr
        , line ( x, y ) ( x2, y2 ) |> colr
        ]


fTrees : Float -> Float -> Float -> Shape msg


open x y angle =
    let
        y2 =
            y + y

        x2 =
            x + y2 * tan (degrees 40)
    in
    group
        [ openPolygon [ ( x, y ), ( x, y ), ( x2, y2 ) ]
            |> colr
        ]



-- initialize branch


view : Collage msg
view =
    collage 300
        300
        [ rect 300 300 |> filled lightGrey
        , graphPaper 10
        , branch 20 20 0
        ]


main =
    graphicsApp { view = view }
