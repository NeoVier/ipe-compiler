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
        ]


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
        ]
