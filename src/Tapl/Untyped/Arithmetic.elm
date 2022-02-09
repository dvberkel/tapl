module Tapl.Untyped.Arithmetic exposing (Term(..), eval, prettyprint, term, toSource)

{-| Untyped Arithmetic Expressions

The language of chapter three of Types and Programming Languages.

-}

import String
import Tapl.Parser exposing (..)



-- Model


{-| The grammar of the untyped arithmetic expressions.
-}
type Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term


{-| The canonical source code for a `Term`.
-}
toSource : Term -> String
toSource t =
    case t of
        TmTrue ->
            "true"

        TmFalse ->
            "false"

        TmIf condition ifcase elsecase ->
            let
                conditionSource =
                    toSource condition

                ifcaseSource =
                    toSource ifcase

                elsecaseSource =
                    toSource elsecase
            in
            List.foldr
                (++)
                ""
                [ "if "
                , conditionSource
                , " then "
                , ifcaseSource
                , " else "
                , elsecaseSource
                ]

        TmZero ->
            "O"

        TmSucc subTerm ->
            "succ " ++ toSource subTerm

        TmPred subTerm ->
            "pred " ++ toSource subTerm

        TmIsZero subTerm ->
            "iszero " ++ toSource subTerm


{-| pretty prints a Term. Values are treated specially
-}
prettyprint : Term -> String
prettyprint t =
    let
        value x =
            case x of
                TmSucc y ->
                    1 + value y

                _ ->
                    0
    in
    case t of
        TmFalse ->
            "F"

        TmTrue ->
            "T"

        TmZero ->
            "0"

        TmSucc _ ->
            String.fromInt <| value t

        _ ->
            toSource t


{-| Parser for an untyped arithmetic expression.
-}
term : Parser Term
term =
    oneOf
        [ true
        , false
        , zero
        , succ
        , pred
        , iszero
        , ifthenelse
        ]


true : Parser Term
true =
    keyword "true"
        |> map (always TmTrue)


false : Parser Term
false =
    keyword "false"
        |> map (always TmFalse)


zero : Parser Term
zero =
    keyword "O"
        |> map (always TmZero)


succ : Parser Term
succ =
    keyword "succ"
        |> andThen spaces
        |> keepRight (lazy (\_ -> term))
        |> map TmSucc


pred : Parser Term
pred =
    keyword "pred"
        |> andThen spaces
        |> keepRight (lazy (\_ -> term))
        |> map TmPred


iszero : Parser Term
iszero =
    keyword "iszero"
        |> andThen spaces
        |> keepRight (lazy (\_ -> term))
        |> map TmIsZero


ifthenelse : Parser Term
ifthenelse =
    keyword "if"
        |> andThen spaces
        |> keepRight (lazy (\_ -> term))
        |> keepLeft spaces
        |> keepLeft (keyword "then")
        |> keepLeft spaces
        |> andThen (lazy (\_ -> term))
        |> keepLeft spaces
        |> keepLeft (keyword "else")
        |> keepLeft spaces
        |> andThen (lazy (\_ -> term))
        |> map (\( ( c, t ), f ) -> TmIf c t f)


{-| Determine if a `Term` is a numerical value.

Used in the evalution relation.

-}
isnumericalval : Term -> Bool
isnumericalval t =
    case t of
        TmZero ->
            True

        TmSucc subTerm ->
            isnumericalval subTerm

        _ ->
            False


{-| Determine if a `Term` is a value.

Similar to `isnumericalval`.

-}
isval : Term -> Bool
isval t =
    case t of
        TmTrue ->
            True

        TmFalse ->
            True

        _ ->
            isnumericalval t


{-| The single-step evaluator

Note that in the implementation in Types and Programming Languages throws a
exception if no rule applies. We will use the `Maybe` type.

-}
eval1 : Term -> Maybe Term
eval1 t =
    let
        mapEval1OfTerm : (Term -> Term) -> Term -> Maybe Term
        mapEval1OfTerm mapping aTerm =
            aTerm
                |> eval1
                |> Maybe.map mapping
    in
    case t of
        TmIf condition ifcase elsecase ->
            case condition of
                TmTrue ->
                    Just ifcase

                TmFalse ->
                    Just elsecase

                _ as subTerm ->
                    mapEval1OfTerm (\s -> TmIf s ifcase elsecase) subTerm

        TmSucc subTerm ->
            mapEval1OfTerm TmSucc subTerm

        TmPred subTerm ->
            case subTerm of
                TmZero ->
                    Just TmZero

                TmSucc s ->
                    if isnumericalval s then
                        Just s

                    else
                        Nothing

                _ as s ->
                    mapEval1OfTerm TmPred s

        TmIsZero subTerm ->
            case subTerm of
                TmZero ->
                    Just TmTrue

                TmSucc s ->
                    if isnumericalval s then
                        Just TmFalse

                    else
                        Nothing

                _ as s ->
                    mapEval1OfTerm TmIsZero s

        _ ->
            Nothing


{-| Determine the normal form of a `Term`.
-}
eval : Term -> Term
eval t =
    case eval1 t of
        Just s ->
            eval s

        Nothing ->
            t
