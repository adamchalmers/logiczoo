module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)

-- MODEL

type alias Model =
    { text:String
    }

init : (Model, Cmd Msg)
init =
    ({text = "LogicZoo!"}, Cmd.none)

-- UPDATE

type Msg = Noop | Btn

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            (model, Cmd.none)
        Btn ->
            ({model | text = "You pressed it"}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ button [onClick Btn] [text model.text]
        ]

-- WIRING

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }