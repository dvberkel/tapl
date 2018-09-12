module Tapl.Untyped.Arithmetic exposing (Term(..), toSource)

{-| Untyped Arithmetic Expressions

The language of chapter three of Types and Programming Languages.

-}

import Parser exposing ((|.), (|=), Parser, andThen, keyword, succeed)



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
toSource term =
    case term of
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


true : Parser Term
true =
    keyword "true"
        |> andThen (\_ -> succeed TmTrue)


false : Parser Term
false =
    keyword "false"
        |> andThen (\_ -> succeed TmFalse)


{-| Determine if a `Term` is a numerical value.

Used in the evalution relation.

-}
isnumericalval : Term -> Bool
isnumericalval term =
    case term of
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
isval term =
    case term of
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
eval1 term =
    let
        mapEval1OfTerm : (Term -> Term) -> Term -> Maybe Term
        mapEval1OfTerm mapping aTerm =
            let
                eval1OfTerm =
                    eval1 aTerm
            in
            Maybe.map mapping eval1OfTerm
    in
    case term of
        TmIf condition ifcase elsecase ->
            case condition of
                TmTrue ->
                    Just ifcase

                TmFalse ->
                    Just elsecase

                _ as subTerm ->
                    mapEval1OfTerm (\t -> TmIf t ifcase elsecase) subTerm

        TmSucc subTerm ->
            mapEval1OfTerm TmSucc subTerm

        TmPred subTerm ->
            case subTerm of
                TmZero ->
                    Just TmZero

                TmSucc t ->
                    if isnumericalval t then
                        Just t

                    else
                        Nothing

                _ as t ->
                    mapEval1OfTerm TmPred t

        TmIsZero subTerm ->
            case subTerm of
                TmZero ->
                    Just TmTrue

                TmSucc t ->
                    if isnumericalval t then
                        Just TmFalse

                    else
                        Nothing

                _ as t ->
                    mapEval1OfTerm TmIsZero t

        _ ->
            Nothing


{-| Determine the normal form of a `Term`.
-}
eval : Term -> Term
eval term =
    case eval1 term of
        Just t ->
            eval t

        Nothing ->
            term
