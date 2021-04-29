module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- MAIN
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }





-- MODEL
type alias Model = 
    { ball : Ball
    , paddle : Paddle
    }
type alias Ball =
    { x : Int
    , y : Int
    , radius : Int
    , vx : Int
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
    | KeyDown PlayerAction
type PlayerAction
    = PaddleLeft
    | PaddleRight





-- INIT 
init : Flags -> ( Model, Cmd Msg )
init _ = 
    ( { ball = initBall
      , paddle = initPaddle
    }
    , Cmd.none  
    )
initBall : Ball
initBall =
    { x = 250
    , y = 250
    , radius = 10
    , vx = 2
    , vy = 4
    }
initPaddle : Paddle
initPaddle = 
    { x = 225
    , y = 480
    , width = 50
    , height = 10
    }





-- VIEW
view : Model -> Svg.Svg Msg
view { ball, paddle } = 
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #d5e7e8"
        ]
        [ viewBall ball
        , viewPaddle paddle
        , rect -- TOP WALL
            [ x "0"
            , y "0"
            , width "500" 
            , height "10"
            , fill "#034e5e"
            ]
            []
        , rect -- LEFT WALL
            [ x "0"
            , y "0"
            , width "10" 
            , height "500"
            , fill "#034e5e"
            ]
            []
        , rect -- RIGHT WALL
            [ x "490"
            , y "0"
            , width "10" 
            , height "500"
            , fill "#034e5e"
            ]
            [] 
        ]

viewBall : Ball -> Svg.Svg Msg
viewBall { x, y } =
    circle 
    [ cx <| String.fromInt x
    , cy <| String.fromInt y
    , fill "#eb7d46"
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
        , fill "#dd6168"
        ]
        []





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
                        |> Debug.log "shouldPaddleBounce"
                
                shouldBounceVertically = 
                    shouldBallBounceVertically model.ball
                shouldBounceHorizontally = 
                    shouldBallBounceHorizontally model.ball
                vx = 
                    if shouldBounceHorizontally then
                        ball.vx * -1
                    else
                        ball.vx
                vy = 
                    if shouldBounce || shouldBounceVertically then
                        ball.vy * -1
                    else
                        ball.vy
                        
                updateBall = 
                     { ball
                        | x = ball.x + vx
                        , y = ball.y + vy
                        , vx = vx
                        , vy = vy
                    }
            in ( { model | ball = updateBall }, Cmd.none )

        KeyDown playerAction ->
            case playerAction of
                PaddleLeft ->
                    ( { model | paddle = model.paddle |> updatePaddle -10}, Cmd.none )
                PaddleRight ->
                    ( { model | paddle = model.paddle |> updatePaddle 10}, Cmd.none )

updatePaddle : Int -> Paddle -> Paddle
updatePaddle amount paddle =
    { paddle | x = paddle.x + amount  |> clamp 10 (490 - paddle.width)}


shouldBallBounce : Paddle -> Ball -> Bool
shouldBallBounce paddle ball = 
    (ball.y + ball.radius >= paddle.y)
        && (ball.x >= paddle.x)
        && (ball.x <= paddle.x + 50)

-- These are for bouncing back off the walls
shouldBallBounceHorizontally : Ball -> Bool
shouldBallBounceHorizontally ball = 
    let 
        radius = ball.radius
    in
        ball.x <= (10 + radius) || ball.x >= (490 - radius)

shouldBallBounceVertically : Ball -> Bool
shouldBallBounceVertically ball = 
    let 
        radius = ball.radius
    in
        ball.y <= (10 + radius) || ball.y >= (490 - radius) -- need to remove the second statement after adding score





-- etc
subscriptions : Model -> Sub Msg
subscriptions _ = 
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        ]

keyDecoder : Decode.Decoder PlayerAction
keyDecoder = 
    Decode.field "key" Decode.string
        |> Decode.andThen keyToPlayerAction

keyToPlayerAction : String -> Decode.Decoder PlayerAction
keyToPlayerAction keyString = 
    case keyString of
        "ArrowLeft" ->
            Decode.succeed PaddleLeft
        "ArrowRight" ->
            Decode.succeed PaddleRight
        _ ->
            Decode.fail "not an event we care about"