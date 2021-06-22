module Plants.Basic exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)



-- Pure Functions


leaf : Float -> Float -> Float -> Float -> Shape msg
leaf width height radius angle =
    roundedRect width height radius
        |> filled darkGreen
        |> rotate (degrees angle)



{--
1. Width, Height, Radius, Angle is for drawing Leaves using roundedRect
2. X1, Y1, X2, Y2 are coordinates for the Trunk using line
3. CX, CY are counters for our Recursion
--}


leaves : ( Float, Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Shape msg
leaves ( width, height, radius ) angle ( x1, y1 ) ( x2, y2 ) ( cx, cy ) =
    group
        [ if cy == distanceFormula x1 y1 x2 y2 then
            group []

          else
            group
                [ leaf width height radius angle |> move ( 10 - cx, cy )
                , leaf width height radius angle |> move ( 10 - cx, cy ) |> mirrorX
                , leaves
                    ( width - 1, height - 1, radius - 1 )
                    (angle - 1)
                    ( x1, y1 )
                    ( x2, y2 )
                    ( cx + 1, cy + 20 )
                ]
        ]


trunk : ( Float, Float ) -> ( Float, Float ) -> Shape msg
trunk ( x1, y1 ) ( x2, y2 ) =
    line ( x1, y1 ) ( x2, y2 ) |> outlined (solid 2) darkBrown


distanceFormula : Float -> Float -> Float -> Float -> Float
distanceFormula x1 y1 x2 y2 =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)



-- initialize branch


view : Collage msg
view =
    collage 300
        300
        [ graphPaper 10
        , trunk ( 0, -20 ) ( 0, 100 )
        , leaves ( 10, 20, 20 ) 120 ( 0, -20 ) ( 0, 100 ) ( 0, 0 )
        ]


main =
    graphicsApp { view = view }



-- Using Records
-- type alias Trunk =
--     { x1 : Float
--     , y1 : Float
--     , x2 : Float
--     , y2 : Float
--     }
-- type alias Leaf =
--     { width : Float
--     , height : Float
--     , radius : Float
--     , angle : Float
--     }
-- initTrunk : Trunk
-- initTrunk =
--     { x1 = 0
--     , y1 = -20
--     , x2 = 0
--     , y2 = 100
--     }
-- initLeaf : Leaf
-- initLeaf =
--     { width = 10
--     , height = 20
--     , radius = 20
--     , angle = 120
--     }
-- viewTrunk : Trunk -> Shape msg
-- viewTrunk trunk =
--     line ( trunk.x1, trunk.y1 ) ( trunk.x2, trunk.y2 ) |> outlined (solid 1) darkBrown
-- viewLeaves : Leaf -> Float -> Float -> Shape msg
-- viewLeaves leaf cx cy =
--     group
--         [ roundedRect leaf.width leaf.height leaf.radius
--             |> filled green
--             |> rotate (degrees leaf.angle)
--             |> move ( 10 - cx, cy )
--         , roundedRect leaf.width leaf.height leaf.radius
--             |> filled green
--             |> rotate (degrees leaf.angle)
--             |> move ( 10 - cx, cy )
--             |> mirrorX
--         ]
-- viewPlant : Trunk -> Leaf -> Float -> Float -> Shape msg
-- viewPlant trunk leaf cx cy =
--     group
--         [ viewTrunk trunk
--         , if cy == distanceFormula trunk.x1 trunk.y1 trunk.x2 trunk.y2 then
--             group []
--           else
--             group
--                 [ viewLeaves leaf cx cy
--                 , viewPlant
--                     trunk
--                     { leaf | width = leaf.width - 1, height = leaf.height - 1, radius = leaf.radius - 1 }
--                     (cx + 1)
--                     (cy + 20)
--                 ]
--         ]
