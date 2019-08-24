module App exposing (main)

import App.Page as Page exposing (Intent(..))
import Browser
import Elm.Ast as Ast
import Html


type alias Model =
    { ast : List Ast.Node
    , code : String
    }


type Msg = Intent Intent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Intent (CodeChanged newCode) ->
            ( { model
                | ast = Ast.fromCode newCode
                , code = newCode
              }
            , Cmd.none
            )


-- Setup


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        code =
            """module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr

type Msg
    = Unknown

type alias Model =
    { count : Int
    , amount : Int
    }

{- A block comment -}
main =
    -- Some inline comment
    Html.text "Hello, World!"
            """
    in
    ( { ast = Ast.fromCode code, code = code }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = Page.view >> Html.map Intent
        }