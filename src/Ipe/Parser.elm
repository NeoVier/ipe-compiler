module Ipe.Parser exposing
    ( moduleDefinition
    , typeDefinition, type_
    )

{-| This is the module responsible for transforming a `String` into an Ipe AST (as in the Ipe.Language module).


## Modules

@docs moduleDefinition


## Types

@docs typeDefinition, type_

-}

import Ipe.Language as Language
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Set exposing (Set)



-------------- RESERVED KEYWORDS --------------


reservedKeywords : Set String
reservedKeywords =
    Set.fromList
        [ "module"
        , "exposing"
        , "type"
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


{-| Every Ipe file must start by defining a module. This function parses a module definition.

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
        , Parser.Extra.nonEmptySequence
            { end = ")"
            , item = exportItem
            , separator = ","
            , spaces = Parser.spaces
            , start = "("
            , trailing = Parser.Forbidden
            }
            { emptyMessage =
                """a module needs to export *something*!

You can either export everything in the module using `(..)`, or just some items by providing a comma-separated list, such as `(function1, function2)`.

If you don't want to expose anything from this module, what's the point of having it?"""
            }
            |> Parser.map Language.ExportSome
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



-------------- TYPE DEFINITION --------------


typeDefinition : Parser Language.TypeDefinition
typeDefinition =
    Parser.succeed Language.TypeDefinition
        |. Parser.keyword "type"
        |. Parser.spaces
        |= uppercaseName
        |. Parser.spaces
        |= Parser.Extra.spaceSeparatedList lowercaseName
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.Extra.nonEmptySequence
            { end = ""
            , item = typeConstructor
            , separator = "|"
            , spaces = Parser.spaces
            , start = ""
            , trailing = Parser.Forbidden
            }
            { emptyMessage =
                """You need to specify at least one type constructor!

Type constructors are names that start with an uppercase letter and are separated by `|`. They may have arguments as well! So you could define types like so:

```
type Fruit
    = Banana
    | Orange
    | Apple
```
"""
            }


{-| -}
typeConstructor : Parser Language.TypeConstructor
typeConstructor =
    Parser.succeed Language.TypeConstructor
        |= uppercaseName
        |. Parser.spaces
        |= Parser.Extra.spaceSeparatedList
            (Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol "("
                    |. Parser.spaces
                    |= type_
                    |. Parser.spaces
                    |. Parser.symbol ")"
                , type_
                ]
            )


type_ : Parser Language.Type
type_ =
    Parser.oneOf
        [ genericType
        , recordType
        , customType
        ]


genericType : Parser Language.Type
genericType =
    lowercaseName
        |> Parser.map (\name -> Language.GenericType { name = name })


recordType : Parser Language.Type
recordType =
    Parser.sequence
        { end = "}"
        , item =
            Parser.succeed Tuple.pair
                |= lowercaseName
                |. Parser.spaces
                |. Parser.symbol ":"
                |. Parser.spaces
                |= Parser.lazy (\_ -> type_)
        , separator = ","
        , spaces = Parser.spaces
        , start = "{"
        , trailing = Parser.Forbidden
        }
        |> Parser.map Language.RecordType


customType : Parser Language.Type
customType =
    Parser.succeed
        (\name argument ->
            Language.CustomType
                { name = name
                , arguments = argument
                }
        )
        |= uppercaseName
        |. Parser.spaces
        |= Parser.Extra.spaceSeparatedList
            (Parser.oneOf
                [ genericType
                , recordType
                , uppercaseName
                    |> Parser.map (\name -> Language.CustomType { name = name, arguments = [] })
                , Parser.succeed identity
                    |. Parser.symbol "("
                    |. Parser.spaces
                    |= Parser.lazy (\_ -> customType)
                    |. Parser.spaces
                    |. Parser.symbol ")"
                ]
            )
