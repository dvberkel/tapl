module Main exposing (main)

import Browser
import Chapter exposing (Chapter)
import Html.Styled as Html exposing (Html, toUnstyled)
import Tapl.Parser as Parser
import Tapl.Untyped.Arithmetic as Arithmetic


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

        context =
            { parser = Parser.complete Arithmetic.term
            , toSource = Arithmetic.toSource
            , eval = Arithmetic.eval
            , prettyprint = Arithmetic.prettyprint
            }

        model =
            Chapter.empty "Untyped Arithmetic" context
                |> Chapter.withSources sources
                |> Model
    in
    Browser.sandbox
        { init = model
        , view = view >> toUnstyled
        , update = update
        }


type alias Model =
    { chapter : Chapter Arithmetic.Term Arithmetic.Term
    }


type Msg
    = UntypedArithmeticMessage Chapter.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UntypedArithmeticMessage m ->
            { model | chapter = Chapter.update m model.chapter }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.map UntypedArithmeticMessage <| Chapter.view model.chapter
        ]
