module Main exposing (main)

import Html
import Parser exposing (deadEndsToString, run)
import Tapl.Untyped.Arithmetic exposing (eval, term, toSource)


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

                canonicalValue =
                    parseResult
                        |> Result.map eval
                        |> Result.map toSource
                        |> Result.withDefault "Nothing"
            in
            Html.div []
                [ Html.span [] [ Html.text <| "\"" ++ source ++ "\"" ]
                , Html.span [] [ Html.text ":" ]
                , Html.span [] [ Html.text canonicalSource ]
                , Html.span [] [ Html.text "→" ]
                , Html.span [] [ Html.text canonicalValue ]
                ]
    in
    Html.div [] (List.map display sources)
