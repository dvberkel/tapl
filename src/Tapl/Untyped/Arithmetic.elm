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
