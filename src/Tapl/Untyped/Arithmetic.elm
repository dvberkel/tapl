module Tapl.Untyped.Arithmetic exposing (Term(..), toSource)

{-| Untyped Arithmetic Expressions

The language of chapter three of Types and Programming Languages.

-}

import Parser exposing ((|.), (|=), Parser, andThen, keyword, lazy, oneOf, spaces, succeed)



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


term : Parser Term
term =
    oneOf
        [ true
        , false
        , zero
        , succ
        ]


true : Parser Term
true =
    succeed TmTrue
        |. keyword "true"


false : Parser Term
false =
    succeed TmFalse
        |. keyword "false"


zero : Parser Term
zero =
    succeed TmZero
        |. keyword "O"


succ : Parser Term
succ =
    succeed TmSucc
        |. keyword "succ"
        |. spaces
        |= lazy (\_ -> term)


pred : Parser Term
pred =
    succeed TmPred
        |. keyword "pred"
        |. spaces
        |= lazy (\_ -> term)


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

        _ as otherTerm ->
            isnumericalval otherTerm


{-| The single-step evaluator

Note that in the implementation in Types and Programming Languages throws a
exception if no rule applies. We will use the a `Maybe` type.

-}
eval1 : Term -> Maybe Term
eval1 t =
    let
        mapEval1OfTerm : (Term -> Term) -> Term -> Maybe Term
        mapEval1OfTerm mapping aTerm =
            let
                eval1OfTerm =
                    eval1 aTerm
            in
            Maybe.map mapping eval1OfTerm
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
