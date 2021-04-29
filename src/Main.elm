module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Process
import Task
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
    , score : Int
    , gameState : GameState
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
    | SleepDone

type PlayerAction
    = PaddleLeft
    | PaddleRight

type GameState
    = Playing
    | Ended



-- INIT 
init : Flags -> ( Model, Cmd Msg )
init _ = 
    ( { ball = initBall
      , paddle = initPaddle
      , score = 0
      , gameState = Playing
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
view { ball, paddle, score } = 
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #d5e7e8"
        ]
        [ viewBall ball
        , viewPaddle paddle
        , viewScore score
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

viewScore : Int -> Svg.Svg Msg
viewScore score =
    g
        [ fontSize "50px"
        , fontFamily "monospace"
        , fill "#034e5e"
        ]
        [ text_ [ x "430", y "60", textAnchor "start" ]
            [ text <| String.fromInt score ]
        ]


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
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
                        
                updatedBall = 
                     { ball
                        | x = ball.x + vx
                        , y = ball.y + vy
                        , vx = vx
                        , vy = vy
                    }
                
                updatedScore =
                    if shouldBounce then
                        updateScore model.score
                    else
                        model.score        

                ( gameState, cmd ) =
                    case getGameState model.ball of 
                        Playing ->
                            ( Playing, Cmd.none )
                        Ended ->
                            let
                                alwaysSleepDone : a -> Msg
                                alwaysSleepDone =
                                    always SleepDone
                                delayCmd = 
                                    Process.sleep 1000
                                        |> Task.perform alwaysSleepDone
                            in
                                ( Ended, delayCmd )
    
            in ( { model | ball = updatedBall, score = updatedScore, gameState = gameState }, cmd )

        KeyDown playerAction ->
            case playerAction of
                PaddleLeft ->
                    ( { model | paddle = model.paddle |> updatePaddle -10}, Cmd.none )
                PaddleRight ->
                    ( { model | paddle = model.paddle |> updatePaddle 10}, Cmd.none )
        
        SleepDone ->
            let _ = Debug.log "restart" "game"
            in ( { model | ball = initBall, score = 0, gameState = Playing }, Cmd.none )

updatePaddle : Int -> Paddle -> Paddle
updatePaddle amount paddle =
    { paddle | x = paddle.x + amount  |> clamp 10 (490 - paddle.width)}

updateScore : Int -> Int
updateScore score =
    score + 1

getGameState : Ball -> GameState
getGameState ball =
    if ball.y >= 550 then
        Ended
    else
        Playing



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
        ball.y <= (10 + radius) 




-- etc
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Playing -> 
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
                , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder) ]
        Ended ->
            Sub.none

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