module Tapl.Context exposing (Context, bind, contains, empty, index, length, lookup, nth)


type Context a
    = Ctx (List ( String, a ))


empty : Context a
empty =
    Ctx []


bind : String -> a -> Context a -> Context a
bind name info (Ctx ctx) =
    ( name, info )
        :: ctx
        |> Ctx


nth : Int -> Context a -> Maybe ( String, a )
nth n (Ctx ctx) =
    ctx
        |> List.drop n
        |> List.head

index : String -> Context a -> Maybe Int
index name (Ctx ctx) =
    case ctx of
        [] -> Nothing

        (needle, _) :: tail ->
            if needle == name then
                Just 0
            else
                index name (Ctx tail)
                |> Maybe.map ((+) 1)


lookup : String -> Context a -> Maybe ( String, a )
lookup name (Ctx ctx) =
    case ctx of
        [] ->
            Nothing

        ( needle, info ) :: tail ->
            if needle == name then
                Just ( name, info )

            else
                lookup name (Ctx tail)


contains : String -> Context a -> Bool
contains name ctx =
    lookup name ctx
        |> Maybe.map (always True)
        |> Maybe.withDefault False


length : Context a -> Int
length (Ctx ctx) =
    List.length ctx
