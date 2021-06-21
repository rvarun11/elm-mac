module Plants.FractalTree_2 exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)


branchColor : Stencil -> Shape msg
branchColor =
    outlined (solid 1) darkGreen



-- leafColor : Stencil -> Shape msg
-- leafColor =
--     filled darkGreen
-- leaf : Float -> Float -> Float -> Float -> Shape msg
-- leaf width height radius angle =
--     roundedRect width height radius
--         |> leafColor
--         |> rotate (degrees angle)


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
            [ openPolygon [ ( newX, newY ), ( x, y ), ( newX, newY ) ] |> branchColor
            , branch newX newY (len * 0.67) (angle - 0.5)
            , branch newX newY (len * 0.67) (angle + 0.5)
            ]



-- initialize branch


trunk : Float -> Float -> Float -> Float -> Shape msg
trunk x1 y1 x2 y2 =
    line ( x1, y1 ) ( x2, y2 ) |> outlined (solid 1) darkBrown


view : Collage msg
view =
    collage 300
        300
        [ graphPaper 10
        , branch 0 0 40 (pi / 2)
        , trunk 0 0 0 40
        ]


main =
    graphicsApp { view = view }
