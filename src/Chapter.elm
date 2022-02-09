module Chapter exposing (..)

import Html exposing (Html)
import Tapl.Parser exposing (Parser)


type Chapter a b
    = Chapter
        { title : String
        , context : Context a b
        , sources : List String
        }


type alias Context a b =
    { parser : Parser a
    , toSource : a -> String
    , eval : a -> b
    , prettyprint : b -> String
    }


empty : String -> Context a b -> Chapter a b
empty title context =
    Chapter { title = title, context = context, sources = [] }


withSources : List String -> Chapter a b -> Chapter a b
withSources sources (Chapter chapter) =
    Chapter { chapter | sources = sources }


addSource : String -> Chapter a b -> Chapter a b
addSource source (Chapter chapter) =
    Chapter { chapter | sources = source :: chapter.sources }


view : Chapter a b -> Html msg
view (Chapter chapter) =
    let
        sources =
            chapter.sources

        parser =
            chapter.context.parser

        toSource =
            chapter.context.toSource

        eval =
            chapter.context.eval

        prettyprint =
            chapter.context.prettyprint

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
    Html.section []
        [ Html.h2 [] [ Html.text chapter.title ]
        , Html.table []
            [ Html.thead []
                [ Html.tr []
                    [ Html.td [] [ Html.text "Source" ]
                    , Html.td [] [ Html.text "Canonical Source" ]
                    , Html.td [] [ Html.text "Canonical Value" ]
                    ]
                ]
            , Html.tbody [] <| List.map display sources
            ]
        ]
