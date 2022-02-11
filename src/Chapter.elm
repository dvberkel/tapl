module Chapter exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Json.Decode as Json
import Tapl.Parser as Parser exposing (Parser)


type alias Id =
    String


type Chapter a
    = Chapter
        { id : Id
        , context : Context a
        , sources : List String
        , input : String
        }


type alias Context a =
    { parser : Parser a
    , toSource : a -> String
    , eval : a -> a
    , prettyprint : a -> String
    }


empty : Id -> Context a -> Chapter a
empty id context =
    Chapter { id = id, context = context, sources = [], input = "" }


withSources : List String -> Chapter a -> Chapter a
withSources sources (Chapter chapter) =
    Chapter { chapter | sources = sources }


addSource : String -> Chapter a -> Chapter a
addSource source (Chapter chapter) =
    Chapter { chapter | sources = source :: chapter.sources }


type Msg
    = UpdateInput Id String
    | KeyDown Id Int


update : Msg -> Chapter a -> Chapter a
update msg (Chapter chapter) =
    case msg of
        UpdateInput id input ->
            if chapter.id == id then
                Chapter { chapter | input = input }

            else
                Chapter chapter

        KeyDown id key ->
            if chapter.id == id && key == 13 then
                case Parser.run chapter.context.parser chapter.input of
                    Just _ ->
                        Chapter
                            { chapter
                                | sources = chapter.input :: chapter.sources
                                , input = ""
                            }

                    Nothing ->
                        Chapter chapter

            else
                Chapter chapter


view : Chapter a -> Html Msg
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

        parseResult =
            chapter.input
                |> Parser.run parser

        inputSource =
            parseResult
                |> Maybe.map toSource
                |> Maybe.withDefault "-"

        inputValue =
            parseResult
                |> Maybe.map eval
                |> Maybe.map prettyprint
                |> Maybe.withDefault "-"
    in
    Html.section []
        [ Html.h2 [] [ Html.text chapter.id ]
        , Html.table
            [ Attribute.css
                [ borderWidth (px 2)
                , borderStyle solid
                , borderColor (rgb 128 128 128)
                , borderSpacing (px 0)
                ]
            ]
            [ Html.thead
                []
                [ Html.tr
                    []
                    [ Html.td
                        [ Attribute.css
                            [ paddingRight (em 1)
                            , borderBottomColor (rgb 0 0 0)
                            , borderBottomStyle solid
                            , borderBottomWidth (px 2)
                            ]
                        ]
                        [ Html.text "Source" ]
                    , Html.td
                        [ Attribute.css
                            [ borderBottomColor (rgb 0 0 0)
                            , borderBottomStyle solid
                            , borderBottomWidth (px 2)
                            ]
                        ]
                        [ Html.text "Canonical Source" ]
                    , Html.td
                        [ Attribute.css
                            [ borderBottomColor (rgb 0 0 0)
                            , borderBottomStyle solid
                            , borderBottomWidth (px 2)
                            ]
                        ]
                        [ Html.text "Canonical Value" ]
                    ]
                , Html.tr
                    []
                    [ Html.td []
                        [ Html.input
                            [ Attribute.type_ "text"
                            , Attribute.value chapter.input
                            , Event.onInput <| UpdateInput chapter.id
                            , onKeyDown <| KeyDown chapter.id
                            ]
                            []
                        ]
                    , Html.td [] [ Html.text inputSource ]
                    , Html.td [] [ Html.text inputValue ]
                    ]
                ]
            , Html.tbody [] <| List.map (viewSource chapter.context) sources
            ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Event.on "keydown" (Json.map tagger Event.keyCode)


viewSource : Context a -> String -> Html msg
viewSource context source =
    let
        parser =
            context.parser

        toSource =
            context.toSource

        eval =
            context.eval

        prettyprint =
            context.prettyprint

        parseResult =
            Parser.run parser source

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
        [ Html.td
            [ Attribute.css
                [ paddingRight (em 1)
                ]
            ]
            [ Html.text source ]
        , Html.td
            [ Attribute.css
                [ paddingRight (em 1)
                ]
            ]
            [ Html.text canonicalSource ]
        , Html.td
            [ Attribute.css
                [ paddingRight (em 1)
                ]
            ]
            [ Html.text canonicalValue ]
        ]
