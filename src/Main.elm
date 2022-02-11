module Main exposing (main)

import Browser
import Chapter exposing (Chapter)
import Html.Styled as Html exposing (Html, toUnstyled)
import Tapl.Context as Context exposing (Context)
import Tapl.Parser as Parser
import Tapl.Untyped.Arithmetic as Arithmetic
import Tapl.Untyped.Lambda as Lambda exposing (NameBind)


main =
    let
        untypedArithmetic =
            Chapter.empty "Untyped Arithmetic"
                { parser = Parser.complete Arithmetic.term
                , toSource = Arithmetic.toSource
                , eval = Arithmetic.eval
                , prettyprint = Arithmetic.prettyprint
                }
                |> Chapter.withSources
                    [ "true"
                    , "false"
                    , "if true then false else true"
                    , "O"
                    , "succ O"
                    , "pred O"
                    , "iszero O"
                    , "if succ O then O else false"
                    ]

        toSource : Lambda.Term -> String
        toSource t =
            t
                |> Lambda.toSource Context.empty
                |> Result.withDefault "-"

        untypedLambda =
            Chapter.empty "Untyped Lambda Calculus"
                { parser = Parser.complete Lambda.term |> Parser.map (\f -> f Context.empty |> Tuple.first)
                , toSource = toSource
                , eval = Lambda.eval Context.empty
                , prettyprint = toSource
                }
                |> Chapter.withSources
                    [ "lambda x.x"
                    , "(lambda x.x)"

                    ]

        model =
            { untypedArithmetic = untypedArithmetic
            , untypedLambda = untypedLambda
            }
    in
    Browser.sandbox
        { init = model
        , view = view >> toUnstyled
        , update = update
        }


type alias Model =
    { untypedArithmetic : Chapter Arithmetic.Term
    , untypedLambda : Chapter Lambda.Term
    }


type Msg
    = UntypedArithmeticMessage Chapter.Msg
    | UntypedLambdaMessage Chapter.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UntypedArithmeticMessage m ->
            { model | untypedArithmetic = Chapter.update m model.untypedArithmetic }

        UntypedLambdaMessage m ->
            { model | untypedLambda = Chapter.update m model.untypedLambda }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.map UntypedArithmeticMessage <| Chapter.view model.untypedArithmetic
        , Html.map UntypedLambdaMessage <| Chapter.view model.untypedLambda
        ]
