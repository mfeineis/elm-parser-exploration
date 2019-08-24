module App.Page exposing (Intent(..), view)

import Elm.Ast as Ast
import Html exposing (Html)
import Html.Attributes as Attr exposing (value)
import Html.Events exposing (onInput)


type Intent
    = CodeChanged String


view : { a | ast : List Ast.Node, code : String } -> Html Intent
view { ast, code } =
    Html.div []
        [ styles
        , Html.div [] [ Html.text "Elm Code" ]
        , Html.textarea
            [ Attr.cols 80
            , onInput CodeChanged
            , Attr.rows 20
            , Attr.value code
            ] []
        , Html.pre [] [ Html.text (Ast.toString ast) ]
        ]
    
styles : Html never
styles =
    Html.node "link"
        [ Attr.href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css"
        , Attr.rel "stylesheet"
        ]
        []