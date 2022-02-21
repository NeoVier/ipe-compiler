module Parser.Extra exposing (nonEmptySequence, spaceSeparatedList)

{-| Helper functions around elm/parser


## Lists

@docs nonEmptySequence, spaceSeparatedList

-}

import Parser exposing ((|.), (|=), Parser)


{-| A function that parses a list of items, but fails if the list is empty
-}
nonEmptySequence :
    { end : String, item : Parser a, separator : String, spaces : Parser (), start : String, trailing : Parser.Trailing }
    -> { emptyMessage : String }
    -> Parser (List a)
nonEmptySequence sequenceArgs { emptyMessage } =
    Parser.sequence sequenceArgs
        |> Parser.andThen
            (\items ->
                if List.isEmpty items then
                    Parser.problem emptyMessage

                else
                    Parser.succeed items
            )


{-| Parse a space-separated list of items
-}
spaceSeparatedList : Parser a -> Parser (List a)
spaceSeparatedList itemParser =
    Parser.loop [] (spaceSeparatedListHelp itemParser)


{-| -}
spaceSeparatedListHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
spaceSeparatedListHelp itemParser items =
    Parser.oneOf
        [ Parser.succeed (\item -> Parser.Loop (item :: items))
            |= itemParser
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse items))
        ]
