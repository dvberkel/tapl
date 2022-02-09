module Main exposing (main)

import Chapter
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
    Chapter.view
        chapter
