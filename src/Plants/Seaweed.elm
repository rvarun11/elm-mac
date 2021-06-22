module Plants.Seaweed exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)


seaweed : ( Float, Float ) -> Shape msg
seaweed ( x, y ) =
    group
        [ curve ( x, y ) [ Pull ( 10, 0 ) ( 0, 0 ) ] |> filled darkGreen
        , curve ( x, y ) [ Pull ( 10, 0 ) ( 0, 0 ) ] |> filled darkGreen |> mirrorX
        ]


seaweed2 : ( Float, Float ) -> ( Float, Float ) -> Shape msg
seaweed2 ( x, y ) ( len, angle ) =
    let
        newX =
            x + len * cos angle

        newY =
            y + len * sin angle
    in
    group
        [ curve ( newX, newY ) [ Pull ( 10, 0 ) ( 0, 0 ) ] |> outlined (solid 1.5) darkGreen
        , curve ( newX, newY ) [ Pull ( 10, 0 ) ( 0, 0 ) ] |> outlined (solid 1.5) darkGreen |> mirrorX
        ]


branches : ( Float, Float, Float ) -> Float -> Float -> Float -> Shape msg
branches ( x, y, len ) angle size c =
    group
        [ if c >= size then
            group []

          else
            group
                [ seaweed2 ( x, y + c ) ( len, angle )
                , branches ( x, y, len - 5 )
                    angle
                    size
                    (c + 20)
                ]
        ]


view : Collage msg
view =
    collage 300
        300
        [ graphPaper 10
        , branches ( 0, 0, 40 ) (pi / 6) 150 0 |> move ( 0, -50 )
        ]


main : GraphicSVG.EllieApp.GraphicsApp
main =
    graphicsApp { view = view }
