module Ipe.Language exposing
    ( ModuleDefinition, ModuleExport(..), ExportItem(..), TypeExport(..)
    , TypeDefinition, TypeConstructor
    , Type(..)
    )

{-| This module defines the AST (Abstract Syntax Tree) of Ipe programs, which means every (valid) Ipe program is represented as a tree.


## Module definition

This is always the first line in an Ipe file. It tells us the name of the module and what types and functions other people can import from it.

@docs ModuleDefinition, ModuleExport, ExportItem, TypeExport


## Types definition

Types are very important! This is how we represent them in the AST

@docs TypeDefinition, TypeConstructor

@docs Type

-}

import Dict



-------------- MODULE DEFINITION --------------


{-| The module definition is the first thing we see when reading an Ipe file. It's the root of the module definition tree, which tells us the module name and all the functions and types the module exports.
-}
type alias ModuleDefinition =
    { moduleName : String, exports : ModuleExport }


{-| A module can export all of the functions and types inside of it, or it can export only some of them.
-}
type ModuleExport
    = ExportEverything
    | ExportSome (List ExportItem)


{-| Each item can either be a function or a type. For functions, it's simple to export them: just type out their name. For types, they can export a different number of variants.
-}
type ExportItem
    = ExportFunction { functionName : String }
    | ExportType { typeName : String, typeExport : TypeExport }


{-| When exporting a type, we can:

  - Export all variants
  - Export no variants (opaque types)
  - Export only some variants

-}
type TypeExport
    = ExportAllVariants
    | ExportNoVariant
    | ExportSomeVariants (List String)



-------------- TYPE DEFINITION --------------


{-| A type can have any number of type variables (which start with a lowercase letter), and it can have any number of constructors. It always needs a name
-}
type alias TypeDefinition =
    { typeName : String
    , typeVariables : List String
    , constructors : List TypeConstructor
    }


{-| A constructor needs a name, and can have any number of arguments, which can be generic or not.
-}
type alias TypeConstructor =
    { constructorName : String
    , constructorArguments : List Type
    }


{-| Types can be:

  - generic: start with a lowercase letter and can have no arguments
  - records: named list of `Type`s
  - custom types: types with arbitrary `Type` arguments

-}
type Type
    = GenericType { name : String }
    | RecordType (Dict.Dict String Type)
    | CustomType { name : String, arguments : List Type }
