module ParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Tapl.Parser as Parser
import Test exposing (..)


suite : Test
suite =
    describe "Parser module"
        [ describe "character"
            [ test "the character parser succeeds when input start with target" <|
                \_ ->
                    let
                        parser =
                            Parser.character 'a'

                        actual =
                            parser "aBCD"

                        expected =
                            [ ( 'a', "BCD" ) ]
                    in
                    Expect.equal actual expected
            , test "the character parser fails when the input does not start with target" <|
                \_ ->
                    let
                        parser =
                            Parser.character 'a'

                        actual =
                            parser "ABCD"

                        expected =
                            []
                    in
                    Expect.equal actual expected
            ]
        , describe "or"
            [ test "or parsers will pass with first alternative" <|
                \_ ->
                    let
                        parser =
                            Parser.character 'a'
                                |> Parser.or (Parser.character 'A')

                        actual =
                            parser "aBCD"

                        expected =
                            [ ( 'a', "BCD" ) ]
                    in
                    Expect.equal actual expected
            , test "or parsers will pass with second alternative" <|
                \_ ->
                    let
                        parser =
                            Parser.character 'a'
                                |> Parser.or (Parser.character 'A')

                        actual =
                            parser "ABCD"

                        expected =
                            [ ( 'A', "BCD" ) ]
                    in
                    Expect.equal actual expected
            ]
        , describe "andThen"
            [ test "andThen parsers will pass with consecutive alternative" <|
                \_ ->
                    let
                        parser =
                            Parser.character 'a'
                                |> Parser.andThen (Parser.character 'b')

                        actual =
                            parser "abCD"

                        expected =
                            [ ( ( 'a', 'b' ), "CD" ) ]
                    in
                    Expect.equal actual expected
            ]
        , describe "many"
            [ test "many parser will pass with no available options" <|
                \_ ->
                    let
                        parser =
                            Parser.many <| Parser.character 'a'

                        actual =
                            parser "BCD"

                        expected =
                            [ ( [], "BCD" ) ]
                    in
                    Expect.equal actual expected
            , test "many parser will pass with no all available options" <|
                \_ ->
                    let
                        parser =
                            Parser.many <| Parser.character 'a'

                        actual =
                            parser "aaaBCD"

                        expected =
                            [ ( [ 'a', 'a', 'a' ], "BCD" )
                            , ( [ 'a', 'a' ], "aBCD" )
                            , ( [ 'a' ], "aaBCD" )
                            , ( [], "aaaBCD" )
                            ]
                    in
                    Expect.equal actual expected
            ]
        ]
