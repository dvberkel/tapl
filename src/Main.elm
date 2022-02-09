module Main exposing (main)

import Tapl.Parser as Parser
import Tapl.Untyped.Arithmetic as Arithmetic
import Visualize


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
    in
    Visualize.table
        { parser = Parser.complete Arithmetic.term
        , toSource = Arithmetic.toSource
        , eval = Arithmetic.eval
        , prettyprint = Arithmetic.prettyprint
        }
        sources
