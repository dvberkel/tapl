module Visualize exposing (Context, table)

import Html exposing (Html, a, b)
import Tapl.Parser exposing (Parser)


type alias Context a b =
    { parser : Parser a
    , toSource : a -> String
    , eval : a -> b
    , prettyprint : b -> String
    }


table : Context a b -> List String -> Html msg
table { parser, toSource, eval, prettyprint } sources =
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
