module Visualize exposing (table)

import Parser exposing (Parser)
import Html exposing (Html)

table: (Parser a) -> (a -> String) -> (a -> b) -> (b -> String) -> List String -> Html msg
table parser toSource eval prettyprint sources =
     let
         display source =
            let
                parseResult =
                    Parser.run parser source

                canonicalSource =
                    case parseResult of
                        Ok term ->
                            toSource term

                        Err deadEnds ->
                            Parser.deadEndsToString deadEnds

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
   