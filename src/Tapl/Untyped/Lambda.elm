module Tapl.Untyped.Lambda exposing (..)

{-| Untyped Lambda Expressions

The language of chapter seven of Types and Programming Languages.

-}

import Browser exposing (application)
import Tapl.Context as Context exposing (Context)
import Tapl.Parser exposing (..)



-- Model


{-| The grammar of the untyped lambda expressions.
-}
type Term
    = TmVar Int Int
    | TmAbs String Term
    | TmApp Term Term


{-|

    For the moment bindings are completely trivial.

-}
type alias NameBind =
    {}


addName : String -> Context NameBind -> Context NameBind
addName name ctx =
    Context.bind name {} ctx


isNameBound : String -> Context NameBind -> Bool
isNameBound name ctx =
    Context.contains name ctx


index2name : Int -> Context NameBind -> Maybe String
index2name n ctx =
    Context.nth n ctx
        |> Maybe.map Tuple.first


name2index : String -> Context NameBind -> Maybe Int
name2index name ctx =
    Context.index name ctx


pickfreshname : String -> Context NameBind -> ( Context NameBind, String )
pickfreshname suggestion ctx =
    if isNameBound suggestion ctx then
        pickfreshname (suggestion ++ "'") ctx

    else
        ( addName suggestion ctx, suggestion )


{-| The canonical source code for a `Term`
-}
toSource : Context NameBind -> Term -> Result Problem String
toSource ctx t =
    case t of
        TmAbs suggestion subTerm ->
            let
                ( context, z ) =
                    pickfreshname suggestion ctx
            in
            case toSource context subTerm of
                Ok source ->
                    Ok <|
                        List.foldr
                            (++)
                            ""
                            [ "(lambda "
                            , z
                            , ". "
                            , source
                            , ")"
                            ]

                Err problem ->
                    Err <|
                        Abstraction problem

        TmApp t1 t2 ->
            case ( toSource ctx t1, toSource ctx t2 ) of
                ( Ok left, Ok right ) ->
                    Ok <|
                        List.foldr
                            (++)
                            ""
                            [ "("
                            , left
                            , " "
                            , right
                            , ")"
                            ]

                ( Err e, Ok _ ) ->
                    Err <| LeftApplication e

                ( Ok _, Err e ) ->
                    Err <| RightApplication e

                ( Err left, Err right ) ->
                    Err <| Application left right

        TmVar index n ->
            if Context.length ctx == n then
                index2name index ctx
                    |> Result.fromMaybe (MissingIndex ctx index)

            else
                Err <| BadIndex ctx index


type Problem
    = BadIndex (Context NameBind) Int
    | MissingIndex (Context NameBind) Int
    | LeftApplication Problem
    | RightApplication Problem
    | Application Problem Problem
    | Abstraction Problem


type alias Factory =
    Context NameBind -> ( Term, Context NameBind )


{-| Parser for a lambda expression.
-}
term : Parser Factory
term =
    oneOf
        [ abstraction
        , application
        , variable
        ]


abstraction : Parser Factory
abstraction =
    oneOf
        [ boundlambda
        , unboundlambda
        ]


boundlambda : Parser Factory
boundlambda =
    keyword "lambda"
        |> andThen spaces
        |> keepRight identifier
        |> keepLeft spaces
        |> keepLeft (character '.')
        |> andThen (lazy (\_ -> term))
        |> map (\( i, t ) -> createAbstraction i t)


identifier : Parser String
identifier =
    let
        alpha =
            predicate Char.isAlpha

        alphaNums =
            predicate Char.isAlphaNum
                |> many
                |> map String.fromList
    in
    alpha
        |> andThen alphaNums
        |> map (\( c, cs ) -> String.cons c cs)


createAbstraction : String -> Factory -> Factory
createAbstraction name factory c0 =
    let
        c1 =
            addName name c0

        ( t, ctx ) =
            factory c1
    in
    ( TmAbs name t, ctx )


unboundlambda : Parser Factory
unboundlambda =
    keyword "lambda"
        |> andThen spaces
        |> andThen (character '_')
        |> andThen spaces
        |> andThen (character '.')
        |> keepRight (lazy (\_ -> term))
        |> map (createAbstraction "_")


application : Parser Factory
application =
    let
        createApplication : ( Factory, Factory ) -> Factory
        createApplication ( left, right ) ctx =
            let
                ( l, _ ) =
                    left ctx

                ( r, _ ) =
                    right ctx
            in
            ( TmApp l r, ctx )
    in
    oneOf
        [ aterm 
            |> andThen aterm
            |> map createApplication
        , aterm
        ]


aterm : Parser Factory
aterm =
    oneOf
        [ character '('
            |> keepRight (lazy (\_ -> term))
            |> keepLeft (character ')')
        ]


variable : Parser Factory
variable =
    let
        createVariable : String -> Factory
        createVariable name ctx =
            let
                index =
                    name2index name ctx
                        |> Maybe.withDefault -1

                -- TODO fix this
            in
            ( TmVar index <| Context.length ctx, ctx )
    in
    identifier
        |> map createVariable


{-| Utilities to evaluate `Term`s
-}
termShift : Int -> Term -> Term
termShift d aTerm =
    let
        walk : Int -> Term -> Term
        walk c t =
            case t of
                TmAbs suggestion subTerm ->
                    TmAbs suggestion <| walk (c + 1) subTerm

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)

                TmVar index n ->
                    if index >= c then
                        TmVar (index + d) (n + d)

                    else
                        TmVar index (n + d)
    in
    walk 0 aTerm


termSubstitute : Int -> Term -> Term -> Term
termSubstitute j substitution aTerm =
    let
        walk : Int -> Term -> Term
        walk c t =
            case t of
                TmAbs suggestion subTerm ->
                    TmAbs suggestion <| walk (c + 1) subTerm

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)

                TmVar index n ->
                    if index == j + c then
                        termShift c substitution

                    else
                        TmVar index n
    in
    walk 0 aTerm


termSubstituteTop : Term -> Term -> Term
termSubstituteTop s t =
    termShift -1 <| termSubstitute 0 (termShift 1 s) t


{-| Determine if a `Term` is a value.
-}
isval : Term -> Bool
isval t =
    case t of
        TmAbs _ _ ->
            True

        _ ->
            False


{-| The single-step evaluator.

Note that in the implementation in Types and Programming Languages throws a
exception if no rules applies. We will use the `Maybe` type.

-}
eval1 : Context NameBind -> Term -> Maybe Term
eval1 ctx t =
    case t of
        TmApp (TmAbs _ l) ((TmAbs _ _) as t2) ->
            Just <| termSubstituteTop t2 l

        TmApp ((TmAbs _ _) as t1) t2 ->
            eval1 ctx t2
                |> Maybe.map (TmApp t1)

        TmApp t1 t2 ->
            eval1 ctx t1
                |> Maybe.map (\x -> TmApp x t2)

        _ ->
            Nothing


eval : Context NameBind -> Term -> Term
eval ctx t =
    case eval1 ctx t of
        Just s ->
            eval ctx s

        Nothing ->
            t
