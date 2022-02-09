module Tapl.Parser exposing (..)


type alias Parser a =
    String -> List ( a, String )


fail : Parser a
fail input =
    []


succeed : a -> Parser a
succeed a input =
    [ ( a, input ) ]


predicate : (Char -> Bool) -> Parser Char
predicate p input =
    let
        match (( c, _ ) as tuple) =
            if p c then
                Just tuple

            else
                Nothing
    in
    String.uncons input
        |> Maybe.andThen match
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


character : Char -> Parser Char
character target =
    predicate (\candidate -> candidate == target)


keyword : String -> Parser String
keyword target input =
    if String.startsWith target input then
        [ ( target, String.dropLeft (String.length target) input ) ]

    else
        []


or : Parser a -> Parser a -> Parser a
or later earlier input =
    List.concat [ earlier input, later input ]


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    List.foldr or fail parsers


andThen : Parser b -> Parser a -> Parser ( a, b )
andThen later earlier input =
    let
        tagWith : a -> ( b, String ) -> ( ( a, b ), String )
        tagWith u ( v, rest ) =
            ( ( u, v ), rest )

        apply : ( a, String ) -> List ( ( a, b ), String )
        apply ( v, rest ) =
            later rest
                |> List.map (tagWith v)
    in
    input
        |> earlier
        |> List.concatMap apply


map : (a -> b) -> Parser a -> Parser b
map f parser input =
    input
        |> parser
        |> List.map (lift f)


lift : (a -> b) -> ( a, String ) -> ( b, String )
lift f =
    Tuple.mapFirst f


lazy : (() -> Parser a) -> Parser a
lazy factory input =
    let
        parser =
            factory ()
    in
    parser input


many : Parser a -> Parser (List a)
many parser =
    let
        p =
            parser
                |> andThen (lazy (\_ -> many parser))
                |> map (\( x, xs ) -> x :: xs)
    in
    p
        |> or (succeed [])


ignore : Parser a -> Parser ()
ignore parser =
    parser
        |> map (always ())


spaces : Parser ()
spaces =
    let
        space =
            character ' '
    in
    ignore <| many space


keepLeft : Parser b -> Parser a -> Parser a
keepLeft later earlier =
    earlier
        |> andThen later
        |> map Tuple.first


keepRight : Parser b -> Parser a -> Parser b
keepRight later earlier =
    earlier
        |> andThen later
        |> map Tuple.second


complete : Parser a -> Parser a
complete parser input =
    input
        |> parser
        |> List.filter (Tuple.second >> String.isEmpty)


run : Parser a -> String -> Maybe a
run parser input =
    input
        |> parser
        |> List.head
        |> Maybe.map Tuple.first
