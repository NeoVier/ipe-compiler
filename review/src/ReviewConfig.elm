module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import CognitiveComplexity
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoInconsistentAliases
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoRedundantConcat
import NoRedundantCons
import NoRegex
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnoptimizedRecursion
import NoUnsortedCases
import NoUnsortedRecords
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify
import UseCamelCase


config : List Rule
config =
    [ NoRegex.rule
    , NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument
    , NoUnsortedRecords.rule NoUnsortedRecords.defaults
    , NoUnsortedCases.rule NoUnsortedCases.defaults
    , NoSimpleLetBody.rule
    , CognitiveComplexity.rule 15
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoPrematureLetComputation.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , Simplify.rule Simplify.defaults
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , UseCamelCase.rule UseCamelCase.default
    , NoModuleOnExposedNames.rule
    , NoBooleanCase.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]

    --, NoInconsistentAliases.config
    --    [ ( "Html.Attributes", "Attr" )
    --    , ( "Json.Decode", "Decode" )
    --    , ( "Json.Encode", "Encode" )
    --    ]
    --    |> NoInconsistentAliases.noMissingAliases
    --    |> NoInconsistentAliases.rule
    ]
