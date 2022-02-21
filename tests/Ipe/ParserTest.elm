module Ipe.ParserTest exposing (suite)

import Expect
import Ipe.Language as Language
import Ipe.Parser
import Parser
import Test exposing (Test, test)


suite : Test
suite =
    Test.describe "The Parser module"
        [ moduleDefinition
        , typeDefinition
        , type_
        ]



-------------- MODULE DEFINITION --------------


moduleDefinition : Test
moduleDefinition =
    let
        expectOk input response =
            Parser.run Ipe.Parser.moduleDefinition input
                |> Expect.equal (Ok response)

        expectErr input =
            Parser.run Ipe.Parser.moduleDefinition input
                |> Expect.err
    in
    Test.describe "Ipe.Parser.moduleDefinition"
        [ test "correctly parses module definition exposing everything" <|
            \_ ->
                let
                    input =
                        "module A exposing (..)"

                    output : Language.ModuleDefinition
                    output =
                        { moduleName = "A", exports = Language.ExportEverything }
                in
                expectOk input output
        , test "correctly parses module definition with some exposed functions" <|
            \_ ->
                let
                    input =
                        "module A exposing (function1, function2)"

                    output : Language.ModuleDefinition
                    output =
                        { moduleName = "A"
                        , exports =
                            Language.ExportSome
                                [ Language.ExportFunction { functionName = "function1" }
                                , Language.ExportFunction { functionName = "function2" }
                                ]
                        }
                in
                expectOk input output
        , test "correctly parses module definition with some exposed types" <|
            \_ ->
                let
                    input =
                        "module A exposing (OpaqueType, SemiOpaqueType(Variant1, Variant2), VisibleType(..))"

                    output : Language.ModuleDefinition
                    output =
                        { moduleName = "A"
                        , exports =
                            Language.ExportSome
                                [ Language.ExportType { typeName = "OpaqueType", typeExport = Language.ExportNoVariant }
                                , Language.ExportType { typeName = "SemiOpaqueType", typeExport = Language.ExportSomeVariants [ "Variant1", "Variant2" ] }
                                , Language.ExportType { typeName = "VisibleType", typeExport = Language.ExportAllVariants }
                                ]
                        }
                in
                expectOk input output
        , test "does not allow modules that start with a lowercase letter" <|
            \_ ->
                let
                    input =
                        "module someModule exposing (..)"
                in
                expectErr input
        , test "does not allow to expose variants on functions" <|
            \_ ->
                let
                    input =
                        "module A exposing (function1(Variant1))"
                in
                expectErr input
        , test "does not allow to expose everything and some things at the same time" <|
            \_ ->
                let
                    input =
                        "module A exposing (.., function1)"
                in
                expectErr input
        , test "does not allow to expose everything and some things at the same time when exposing function first" <|
            \_ ->
                let
                    input =
                        "module A exposing (function1, ..)"
                in
                expectErr input
        , test "does not allow module to expose nothing" <|
            \_ ->
                let
                    input =
                        "module A"
                in
                expectErr input
        , test "does not allow module to expose nothing, with the exposes keyword" <|
            \_ ->
                let
                    input =
                        "module A exposing ()"
                in
                expectErr input
        , test "does not allow module to expose nothing, with the exposes keyword without parenthesis" <|
            \_ ->
                let
                    input =
                        "module A exposing"
                in
                expectErr input
        ]



-------------- TYPE DEFINITION --------------


typeDefinition : Test
typeDefinition =
    let
        expectOk input response =
            Parser.run Ipe.Parser.typeDefinition input
                |> Expect.equal (Ok response)

        expectErr input =
            Parser.run Ipe.Parser.typeDefinition input
                |> Expect.err
    in
    Test.describe "Ipe.Parser.typeDefinition"
        [ test "correctly errors out on incomplete type definition" <|
            \_ ->
                let
                    input =
                        "type Fruit = "
                in
                expectErr input
        , test "correctly parses a simple union type" <|
            \_ ->
                let
                    input =
                        "type Fruit = Apple | Banana | Orange"

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = []
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments = []
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            , { constructorName = "Orange", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a simple union type with variables" <|
            \_ ->
                let
                    input =
                        "type Fruit season currency = Apple | Banana | Orange"

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = [ "season", "currency" ]
                        , constructors =
                            [ { constructorName = "Apple", constructorArguments = [] }
                            , { constructorName = "Banana", constructorArguments = [] }
                            , { constructorName = "Orange", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a type with constructors that have simple arguments" <|
            \_ ->
                let
                    input =
                        """type Fruit
                            = Apple Int
                            | Banana
                        """

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = []
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments =
                                    [ Language.CustomType
                                        { name = "Int"
                                        , arguments = []
                                        }
                                    ]
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a type with constructors that have generic arguments" <|
            \_ ->
                let
                    input =
                        """type Fruit argument
                            = Apple argument
                            | Banana
                        """

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = [ "argument" ]
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments = [ Language.GenericType { name = "argument" } ]
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a type with constructors that have complex arguments" <|
            \_ ->
                let
                    input =
                        """type Fruit
                            = Apple (Maybe Int)
                            | Banana
                        """

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = []
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments =
                                    [ Language.CustomType
                                        { name = "Maybe"
                                        , arguments =
                                            [ Language.CustomType
                                                { name = "Int"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    ]
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a type with constructors that have simple record arguments" <|
            \_ ->
                let
                    input =
                        """type Fruit
                            = Apple { kind : String, price : Int }
                            | Banana
                        """

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = []
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments =
                                    [ Language.RecordType
                                        [ ( "kind", Language.CustomType { name = "String", arguments = [] } )
                                        , ( "price", Language.CustomType { name = "Int", arguments = [] } )
                                        ]
                                    ]
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a type with constructors that have complex record arguments" <|
            \_ ->
                let
                    input =
                        """type Fruit
                            = Apple { kind : Maybe String, price : Currency USD }
                            | Banana
                        """

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = []
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments =
                                    [ Language.RecordType
                                        [ ( "kind"
                                          , Language.CustomType
                                                { name = "Maybe"
                                                , arguments =
                                                    [ Language.CustomType { name = "String", arguments = [] }
                                                    ]
                                                }
                                          )
                                        , ( "price"
                                          , Language.CustomType
                                                { name = "Currency"
                                                , arguments =
                                                    [ Language.CustomType { name = "USD", arguments = [] }
                                                    ]
                                                }
                                          )
                                        ]
                                    ]
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        , test "correctly parses a type with constructors that have very complex arguments" <|
            \_ ->
                let
                    input =
                        """type Fruit currency unit
                            = Apple (SomeConstructor { kind : Maybe String, price : Currency currency (Coins unit) }) { someField : currency }
                            | Banana
                        """

                    output : Language.TypeDefinition
                    output =
                        { typeName = "Fruit"
                        , typeVariables = [ "currency", "unit" ]
                        , constructors =
                            [ { constructorName = "Apple"
                              , constructorArguments =
                                    [ Language.CustomType
                                        { name = "SomeConstructor"
                                        , arguments =
                                            [ Language.RecordType
                                                [ ( "kind"
                                                  , Language.CustomType
                                                        { name = "Maybe"
                                                        , arguments =
                                                            [ Language.CustomType { name = "String", arguments = [] }
                                                            ]
                                                        }
                                                  )
                                                , ( "price"
                                                  , Language.CustomType
                                                        { name = "Currency"
                                                        , arguments =
                                                            [ Language.GenericType { name = "currency" }
                                                            , Language.CustomType
                                                                { name = "Coins"
                                                                , arguments = [ Language.GenericType { name = "unit" } ]
                                                                }
                                                            ]
                                                        }
                                                  )
                                                ]
                                            ]
                                        }
                                    , Language.RecordType
                                        [ ( "someField"
                                          , Language.GenericType { name = "currency" }
                                          )
                                        ]
                                    ]
                              }
                            , { constructorName = "Banana", constructorArguments = [] }
                            ]
                        }
                in
                expectOk input output
        ]


type_ : Test
type_ =
    let
        expectOk input output =
            Parser.run Ipe.Parser.type_ input
                |> Expect.equal (Ok output)
    in
    Test.describe "Ipe.Parser.type_"
        [ test "correctly parses generic type" <|
            \_ ->
                let
                    input =
                        "a"

                    output =
                        Language.GenericType { name = "a" }
                in
                expectOk input output
        , test "correctly parses simple custom type" <|
            \_ ->
                let
                    input =
                        "Int"

                    output =
                        Language.CustomType { name = "Int", arguments = [] }
                in
                expectOk input output
        , test "correctly parses custom type with one argument" <|
            \_ ->
                let
                    input =
                        "Maybe Int"

                    output =
                        Language.CustomType
                            { name = "Maybe"
                            , arguments =
                                [ Language.CustomType { name = "Int", arguments = [] }
                                ]
                            }
                in
                expectOk input output
        , test "correctly parses custom type with one generic argument" <|
            \_ ->
                let
                    input =
                        "Maybe a"

                    output =
                        Language.CustomType
                            { name = "Maybe"
                            , arguments =
                                [ Language.GenericType { name = "a" }
                                ]
                            }
                in
                expectOk input output
        , test "correctly parses custom type with two arguments" <|
            \_ ->
                let
                    input =
                        "Result String Int"

                    output =
                        Language.CustomType
                            { name = "Result"
                            , arguments =
                                [ Language.CustomType { name = "String", arguments = [] }
                                , Language.CustomType { name = "Int", arguments = [] }
                                ]
                            }
                in
                expectOk input output
        , test "correctly parses complex custom type" <|
            \_ ->
                let
                    input =
                        "Result (Maybe Int) String"

                    output =
                        Language.CustomType
                            { name = "Result"
                            , arguments =
                                [ Language.CustomType
                                    { name = "Maybe"
                                    , arguments =
                                        [ Language.CustomType { name = "Int", arguments = [] }
                                        ]
                                    }
                                , Language.CustomType { name = "String", arguments = [] }
                                ]
                            }
                in
                expectOk input output
        , test "correctly parses record type" <|
            \_ ->
                let
                    input =
                        "{ a : Int, b : b }"

                    output =
                        Language.RecordType
                            [ ( "a", Language.CustomType { name = "Int", arguments = [] } )
                            , ( "b", Language.GenericType { name = "b" } )
                            ]
                in
                expectOk input output
        , test "correctly parses custom type with a record argument" <|
            \_ ->
                let
                    input =
                        "Result { a : Int, b : b } String"

                    output =
                        Language.CustomType
                            { name = "Result"
                            , arguments =
                                [ Language.RecordType
                                    [ ( "a", Language.CustomType { name = "Int", arguments = [] } )
                                    , ( "b", Language.GenericType { name = "b" } )
                                    ]
                                , Language.CustomType { name = "String", arguments = [] }
                                ]
                            }
                in
                expectOk input output
        ]
