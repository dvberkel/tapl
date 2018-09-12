module Tapl.Untyped.Arithmetic exposing (Term(..))

{-| Untyped Arithmetic Expressions

The language of chapter three of Types and Programming Languages.

-}


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

Note that in the implementation in Types and Programming Languages throws a exception if no rule applies. We will use the a `Maybe` type.

-}
eval1 : Term -> Maybe Term
eval1 term =
    case term of
        TmIf condition ifcase elsecase ->
            case condition of
                TmTrue ->
                    Just ifcase

                TmFalse ->
                    Just elsecase

                _ as subTerm ->
                    let
                        eval1SubTerm =
                            eval1 subTerm

                        toTmIf : Term -> Term
                        toTmIf t =
                            TmIf t ifcase elsecase
                    in
                    Maybe.map toTmIf eval1SubTerm

        TmSucc subTerm ->
            let
                eval1SubTerm =
                    eval1 subTerm

                toTmSucc : Term -> Term
                toTmSucc t =
                    TmSucc t
            in
            Maybe.map toTmSucc eval1SubTerm

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
                    let
                        eval1SubTerm =
                            eval1 t

                        toTmPred : Term -> Term
                        toTmPred s =
                            TmPred s
                    in
                    Maybe.map toTmPred eval1SubTerm

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
                    let
                        eval1SubTerm =
                            eval1 t

                        toTmIsZero : Term -> Term
                        toTmIsZero s =
                            TmIsZero s
                    in
                    Maybe.map toTmIsZero eval1SubTerm

        _ ->
            Nothing
