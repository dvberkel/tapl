module Main exposing (main)

import Html
import Parser exposing (deadEndsToString, run)
import Tapl.Untyped.Arithmetic exposing (term, toSource)


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
            ]

        display source =
            let
                parseResult =
                    run term source

                canonicalSource =
                    case parseResult of
                        Ok term ->
                            toSource term

                        Err deadEnds ->
                            deadEndsToString deadEnds
            in
            Html.div []
                [ Html.span [] [ Html.text source ]
                , Html.span [] [ Html.text ":" ]
                , Html.span [] [ Html.text canonicalSource ]
                ]
    in
    Html.div [] (List.map display sources)
