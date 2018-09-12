module Main exposing (main)

import Html

import Tapl.Untyped.Arithmetic exposing (..)

main =
    let
        terms =
            [TmTrue
            , TmFalse
            , TmIf TmTrue TmFalse TmTrue
            , TmZero
            , TmSucc TmZero
            , TmPred TmZero
            , TmIsZero TmZero
            ]

        content term =
            Html.div [] [Html.span [] [Html.text (toSource term)]]
    in
        Html.div [] (List.map content terms)
