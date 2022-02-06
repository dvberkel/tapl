module Main exposing (main)

import Html
import Parser exposing (deadEndsToString, run)
import Tapl.Untyped.Arithmetic exposing (eval, prettyprint, term, toSource)


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

                canonicalValue =
                    parseResult
                        |> Result.map eval
                        |> Result.map prettyprint
                        |> Result.withDefault "Nothing"
            in
            Html.tr []
                [ Html.td [] [ Html.text source ]
                , Html.td [] [ Html.text canonicalSource ]
                , Html.td [] [ Html.text canonicalValue ]
                ]
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.td [] [ Html.text "Source" ]
                , Html.td [] [ Html.text "Canonical Source" ]
                , Html.td [] [ Html.text "Canonical Value" ]
                ]
            ]
        , Html.tbody [] <| List.map display sources
        ]
