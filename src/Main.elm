module Main exposing (main)

import Browser
import Browser.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser.Events exposing (onAnimationFrame)   


-- MODEL
type alias Model = 
    { ball : Ball
    , paddle : Paddle
    }
type alias Ball =
    { x : Int
    , y : Int
    , radius : Int
    , vy : Int
    }

type alias Paddle =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }
type alias Flags = ()
type Msg
    = OnAnimationFrame Float  -- it takes a float which is the no. of ms since the previous animation frame 
 
init : Flags -> ( Model, Cmd Msg )
init _ = 
    ( { ball = initBall
      , paddle = initPaddle
    }
    , Cmd.none  
    )

-- INIT
initBall : Ball
initBall =
    { x = 250
    , y = 250
    , radius = 10
    , vy = 4
    }

initPaddle : Paddle
initPaddle = 
    { x = 225
    , y = 480
    , width = 50
    , height = 10
    }

-- MAIN
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model = 
    case msg of
        OnAnimationFrame timeDelta ->
            let
                ball =  
                    model.ball
                shouldBounce =
                    shouldBallBounce model.paddle model.ball
                        |> Debug.log "shouldBounce"
                
                vy = 
                    if shouldBounce then
                        ball.vy * -1
                    else
                        ball.vy
                updateBall = 
                     { ball
                        | y = ball.y + vy
                        , vy = vy
                    }
            in ( { model | ball = updateBall }, Cmd.none )

shouldBallBounce : Paddle -> Ball -> Bool
shouldBallBounce paddle ball = 
    (ball.y + ball.radius >= paddle.y)
        && (ball.x >= paddle.x)
        && (ball.x <= paddle.x + 50)
-- VIEW 
view : Model -> Svg.Svg Msg
view { ball, paddle } = 
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball
        , viewPaddle paddle
        ]

viewBall : Ball -> Svg.Svg Msg
viewBall { x, y } =
    circle 
    [ cx <| String.fromInt x
    , cy <| String.fromInt y
    , fill "green"
    , stroke "black"
    , r "10"
    ]
    []

viewPaddle : Paddle -> Svg.Svg Msg
viewPaddle paddle =
    rect 
        [ x <| String.fromInt paddle.x
        , y <| String.fromInt paddle.y
        , width <| String.fromInt paddle.width
        , height <| String.fromInt paddle.height
        ]
        []
subscriptions : Model -> Sub Msg
subscriptions _ = 
    Browser.Events.onAnimationFrameDelta OnAnimationFrame

