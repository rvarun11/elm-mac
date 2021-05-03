module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)



-- DEFINITIONS


type alias Model =
    { ball : Ball
    , paddle : Paddle
    , score : String
    , gameState : GameState
    , time : Float
    }


type alias Ball =
    { x : Float
    , y : Float
    , radius : Float
    , vx : Float
    , vy : Float
    }


type alias Paddle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type GameState
    = Playing
    | Ended


type Msg
    = Tick Float GetKeyState



-- INIT


init : Model
init =
    { ball = initBall
    , paddle = initPaddle
    , score = "0"
    , gameState = Playing
    , time = 0
    }


initBall : Ball
initBall =
    { x = 0
    , y = 0
    , radius = 10
    , vx = 50
    , vy = 100
    }


initPaddle : Paddle
initPaddle =
    { x = 0
    , y = -190
    , width = 50
    , height = 10
    }



-- VIEW


view : Model -> Collage Msg
view { ball, paddle, score } =
    collage 400
        400
        [ rect 400 400 |> filled lightGrey |> makeTransparent 0.5
        , openPolygon [ ( -200, -200 ), ( -200, 200 ), ( 200, 200 ), ( 200, -200 ) ] |> outlined (solid 15) darkBlue
        , viewBall ball
        , viewPaddle paddle
        , viewScore score
        ]


viewBall : Ball -> Shape Msg
viewBall ball =
    circle ball.radius
        |> filled orange
        |> move ( ball.x, ball.y)


viewPaddle : Paddle -> Shape Msg
viewPaddle paddle =
    roundedRect paddle.width paddle.height 5
        |> filled lightRed
        |> move ( paddle.x, paddle.y )


viewScore : String -> Shape Msg
viewScore score =
    text score
        |> filled darkBlue
        |> scale 4
        |> move ( 130, 140 )



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t ( getKeyState, _, _ ) ->
            let
                ball =
                    model.ball

                timeChange =
                    t - model.time


                xNew = ball.x + ball.vx * timeChange
                yNew = ball.y + ball.vy * timeChange

                vNew =
                    if xNew <= -180 || xNew >= 180
                    then -ball.vx
                    else  ball.vx

                (newScore, vyNew) = 
                    if yNew <= -180
                        && ball.x >= model.paddle.x - 25 
                        && ball.x <= model.paddle.x + 25
                    then (updateScore model.score, -ball.vy)
                    else if yNew >= 180 -- for upper wall
                    then (model.score, -ball.vy)
                    else (model.score, ball.vy)
                
                updatedBall =
                    { ball
                        | x = xNew
                        , y = yNew
                        , vx = vNew
                        , vy = vyNew
                    }

            in
            case getGameState ball of
                Playing ->
                    { model
                        | time = t
                        , ball = updatedBall
                        , score = newScore
                        , paddle =
                            if getKeyState LeftArrow == Down then
                                model.paddle |> updatePaddle -5

                            else if getKeyState RightArrow == Down then
                                model.paddle |> updatePaddle 5

                            else
                                model.paddle
                    }

                Ended ->
                    { model
                        | time = t
                        , ball = initBall
                        , score = "0"
                        , paddle = initPaddle
                        , gameState = Playing
                    }


updatePaddle : Float -> Paddle -> Paddle
updatePaddle amount paddle =
    { paddle | x = paddle.x + amount |> clamp (-217 + paddle.width) (217 - paddle.width) }


updateScore : String -> String
updateScore score =
    String.fromInt (Maybe.withDefault 0 (String.toInt score) + 1)


getGameState : Ball -> GameState
getGameState ball =
    if ball.y <= -220 then
        Ended

    else
        Playing



-- MAIN


main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        , title = "1P Squash"
        }
