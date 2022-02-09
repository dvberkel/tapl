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
        (Parser.complete Arithmetic.term)
        Arithmetic.toSource
        Arithmetic.eval
        Arithmetic.prettyprint
        sources
