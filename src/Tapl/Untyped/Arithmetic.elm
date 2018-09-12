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
