module Plants.Tulsi exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)
import Svg.Attributes exposing (fill)



-- initialize branch


trunk : ( Float, Float ) -> ( Float, Float ) -> Shape msg
trunk ( x1, y1 ) ( x2, y2 ) =
    line ( x1, y1 ) ( x2, y2 ) |> outlined (solid 1) darkBrown


leaves : ( Float, Float ) -> Shape msg
leaves ( x, y ) =
    group
        [ curve ( 0, 0 )
            [ Pull ( 0, 5 ) ( 10, 0 )
            , Pull ( 0, -5 ) ( 0, 0 )
            , Pull ( 0 - 5, 0 ) ( 0, 10 )
            , Pull ( 0 + 5, 0 ) ( 0, 0 )
            , Pull ( 0 - 5, -5 ) ( -10, 0 )
            , Pull ( 0, 5 ) ( 0, 0 )
            ]
            |> filled darkGreen
            |> rotate (degrees 135)
            |> move ( x, y )
        , curve ( 0, 0 )
            [ Pull ( 0, 5 ) ( 10, 0 )
            , Pull ( 0, -5 ) ( 0, 0 )
            , Pull ( 0 - 5, 0 ) ( 0, 10 )
            , Pull ( 0 + 5, 0 ) ( 0, 0 )
            , Pull ( 0 - 5, -5 ) ( -10, 0 )
            , Pull ( 0, 5 ) ( 0, 0 )
            ]
            |> filled darkGreen
            |> rotate (degrees 145)
            |> move ( x, y )
            |> mirrorX
        ]


branch : ( Float, Float ) -> ( Float, Float ) -> Shape msg
branch ( x, y ) ( len, angle ) =
    let
        newX =
            x + len * cos angle

        newY =
            y + len * sin angle
    in
    group
        [ curve ( x, y ) [ Pull ( newX, newY ) ( newX - 3, newY + 5 ) ] |> outlined (solid 1) darkBrown
        , curve ( x, y ) [ Pull ( newX, newY ) ( newX - 3, newY + 5 ) ] |> outlined (solid 1) darkBrown |> mirrorX
        , leaves ( newX - 3, newY + 5 )
        ]


branches : ( Float, Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float ) -> Float -> Shape msg
branches ( x, y, len ) angle ( tx1, ty1 ) ( tx2, ty2 ) cy =
    group
        [ if cy == distanceFormula ( tx1, ty1 ) ( tx2, ty2 ) then
            group []

          else
            group
                [ branch ( x, y + cy ) ( len, angle )
                , branches ( x, y, len - 5 )
                    angle
                    ( tx1, ty1 )
                    ( tx2, ty2 )
                    (cy + 20)
                ]
        ]


distanceFormula : ( Float, Float ) -> ( Float, Float ) -> Float
distanceFormula ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


view : Collage msg
view =
    collage 300
        300
        [ graphPaper 10
        , group
            [ trunk ( 0, 0 ) ( 0, 100 )
            , branches ( 0, 10, 40 ) (pi / 4) ( 0, 0 ) ( 0, 100 ) 0
            ]
        ]


main : GraphicSVG.EllieApp.GraphicsApp
main =
    graphicsApp { view = view }
