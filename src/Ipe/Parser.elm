module Ipe.Parser exposing (moduleDefinition)

{-| This is the module responsible for transforming a `String` into an Ipe AST (as in the Ipe.Language module).
-}

import Ipe.Language as Language
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)



-------------- RESERVED KEYWORDS --------------


reservedKeywords : Set String
reservedKeywords =
    Set.fromList
        [ "module"
        , "exposing"
        ]



-------------- HELPER FUNCTIONS --------------


{-| Parse a name that starts with an uppercase letter. Useful for module names and type names.
-}
uppercaseName : Parser String
uppercaseName =
    Parser.variable
        { inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedKeywords
        , start = Char.isUpper
        }


{-| Parse a name that starts with a lowercase letter. Useful for function names and variable names.
-}
lowercaseName : Parser String
lowercaseName =
    Parser.variable
        { inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedKeywords
        , start = Char.isLower
        }



-------------- MODULE DEFINITION --------------


{-| Parse a module definition.

    -- Modules can expose some definitions
    module ModuleName exposing (OpaqueType, SemiOpaqueType(Variant1, Variant2), VisibleType(..), function)


    -- Or all of them
    module ModuleName exposing (..)

-}
moduleDefinition : Parser Language.ModuleDefinition
moduleDefinition =
    Parser.succeed Language.ModuleDefinition
        |. Parser.keyword "module"
        |. Parser.spaces
        |= uppercaseName
        |. Parser.spaces
        |. Parser.keyword "exposing"
        |. Parser.spaces
        |= moduleExport


{-| -}
moduleExport : Parser Language.ModuleExport
moduleExport =
    Parser.oneOf
        [ Parser.succeed Language.ExportEverything
            |. Parser.token "(..)"
        , Parser.sequence
            { end = ")"
            , item = exportItem
            , separator = ","
            , spaces = Parser.spaces
            , start = "("
            , trailing = Parser.Forbidden
            }
            |> Parser.andThen
                (\exportedItems ->
                    if List.isEmpty exportedItems then
                        Parser.problem
                            """a module needs to export *something*!

You can either export everything in the module using `(..)`, or just some items by providing a comma-separated list, such as `(function1, function2)`.

If you don't want to expose anything from this module, what's the point of having it?"""

                    else
                        Parser.succeed (Language.ExportSome exportedItems)
                )
        ]


{-| -}
exportItem : Parser Language.ExportItem
exportItem =
    Parser.oneOf
        [ lowercaseName
            |> Parser.map
                (\functionName ->
                    Language.ExportFunction { functionName = functionName }
                )
        , Parser.succeed
            (\typeName typeExport_ ->
                Language.ExportType
                    { typeName = typeName
                    , typeExport = typeExport_
                    }
            )
            |= uppercaseName
            |= typeExport
        ]


{-| -}
typeExport : Parser Language.TypeExport
typeExport =
    Parser.oneOf
        [ Parser.succeed Language.ExportAllVariants
            |. Parser.token "(..)"
        , Parser.sequence
            { end = ")"
            , item = uppercaseName
            , separator = ","
            , spaces = Parser.spaces
            , start = "("
            , trailing = Parser.Forbidden
            }
            |> Parser.map Language.ExportSomeVariants
        , Parser.succeed Language.ExportNoVariant
        ]
