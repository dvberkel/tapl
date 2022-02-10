module Main exposing (main)

import Browser
import Chapter
import Html.Styled exposing (toUnstyled)
import Tapl.Parser as Parser
import Tapl.Untyped.Arithmetic as Arithmetic


main =
    let
        sources =
            [ "true"
            , "false"
            , "if true then false else true"
            , "O"
            , "succ O"
            , "pred O"
            , "iszero O"
            , "if succ O then O else false"
            ]

        context =
            { parser = Parser.complete Arithmetic.term
            , toSource = Arithmetic.toSource
            , eval = Arithmetic.eval
            , prettyprint = Arithmetic.prettyprint
            }

        chapter =
            Chapter.empty "Untyped Arithmetic" context
                |> Chapter.withSources sources
    in
    Browser.sandbox
        { init = chapter
        , view = Chapter.view >> toUnstyled
        , update = Chapter.update
        }
