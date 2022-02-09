module Visualize exposing (table)

import Html exposing (Html)
import Tapl.Parser exposing (Parser)


table : Parser a -> (a -> String) -> (a -> b) -> (b -> String) -> List String -> Html msg
table parser toSource eval prettyprint sources =
    let
        display source =
            let
                parseResult =
                    Tapl.Parser.run parser source

                canonicalSource =
                    parseResult
                        |> Maybe.map toSource
                        |> Maybe.withDefault "parse problem"

                canonicalValue =
                    parseResult
                        |> Maybe.map eval
                        |> Maybe.map prettyprint
                        |> Maybe.withDefault "Nothing"
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
