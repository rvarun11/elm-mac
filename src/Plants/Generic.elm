module Plants.Generic exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (graphicsApp)


type alias Trunk =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


type alias Leaf =
    { width : Float
    , height : Float
    , radius : Float
    , angle : Float
    }


initTrunk : Trunk
initTrunk =
    { x1 = 0
    , y1 = -20
    , x2 = 0
    , y2 = 100
    }


initLeaf : Leaf
initLeaf =
    { width = 10
    , height = 20
    , radius = 20
    , angle = 120
    }


viewTrunk : Trunk -> Shape msg
viewTrunk trunk =
    line ( trunk.x1, trunk.y1 ) ( trunk.x2, trunk.y2 ) |> outlined (solid 1) darkBrown


viewLeaves : Leaf -> Float -> Float -> Shape msg
viewLeaves leaf cx cy =
    group
        [ roundedRect leaf.width leaf.height leaf.radius
            |> filled green
            |> rotate (degrees leaf.angle)
            |> move ( 10 - cx, cy )
        , roundedRect leaf.width leaf.height leaf.radius
            |> filled green
            |> rotate (degrees leaf.angle)
            |> move ( 10 - cx, cy )
            |> mirrorX
        ]


viewPlant : Trunk -> Leaf -> Float -> Float -> Shape msg
viewPlant trunk leaf cx cy =
    group
        [ viewTrunk trunk
        , if cy == dist trunk.x1 trunk.y1 trunk.x2 trunk.y2 then
            group []

          else
            group
                [ viewLeaves leaf cx cy
                , viewPlant
                    trunk
                    { leaf | width = leaf.width - 1, height = leaf.height - 1, radius = leaf.radius - 1 }
                    (cx + 1)
                    (cy + 20)
                ]
        ]


dist : Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)



-- initialize branch


view : Collage msg
view =
    collage 300
        300
        [ rect 300 300 |> filled white
        , graphPaper 10
        , viewPlant initTrunk initLeaf 0 0
        ]


main =
    graphicsApp { view = view }
