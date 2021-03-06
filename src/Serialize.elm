module Serialize exposing
    ( recipeFromString
    , shoppingListFromString
    , shoppingListSourcesFromString
    )

import Parser as P exposing ((|.), (|=), Parser)
import ParserExtras
import Recipe exposing (Recipe)
import Result
import ShoppingList exposing (ShoppingList)


recipeFromString : String -> Result String Recipe
recipeFromString =
    P.run recipe >> Result.mapError ParserExtras.deadEndsToString


shoppingListFromString : String -> Result String ShoppingList
shoppingListFromString =
    P.run shoppingList >> Result.mapError ParserExtras.deadEndsToString


shoppingListSourcesFromString : String -> Result String (List ShoppingList.Source)
shoppingListSourcesFromString =
    P.run shoppingListSources >> Result.mapError ParserExtras.deadEndsToString


recipe : Parser Recipe
recipe =
    P.succeed Recipe
        |= P.getChompedString (P.chompWhile (\c -> c /= '\n'))
        |. P.token "\n\n"
        |= P.loop []
            (\ingredients ->
                P.oneOf
                    [ P.token "\n"
                        |> P.map (\_ -> P.Done (List.reverse ingredients))
                    , ingredient
                        |> P.map (\i -> P.Loop (i :: ingredients))
                    ]
            )
        |= P.getChompedString (P.chompWhile (\_ -> True))


ingredient : Parser Recipe.Ingredient
ingredient =
    P.succeed Recipe.Ingredient
        |= stringWithoutCommas
        |. P.token ", "
        |= stringWithoutCommas
        |= (try <|
                P.succeed identity
                    |. P.token ", "
                    |= string
           )
        |. P.token "\n"


shoppingList : Parser ShoppingList
shoppingList =
    P.succeed ShoppingList
        |= P.loop []
            (\items ->
                P.oneOf
                    [ shoppingListItem
                        |> P.map (\i -> P.Loop (i :: items))
                    , P.succeed ()
                        |> P.map (\_ -> P.Done (List.reverse items))
                    ]
            )


shoppingListItem : Parser ShoppingList.Item
shoppingListItem =
    P.succeed ShoppingList.Item
        |= stringWithoutCommas
        |. P.token ", "
        |= string
        |. P.chompWhile (\c -> c == '\n')


shoppingListSources : Parser (List ShoppingList.Source)
shoppingListSources =
    P.loop []
        (\items ->
            P.oneOf
                [ shoppingListSource
                    |> P.map (\i -> P.Loop (i :: items))
                , P.succeed ()
                    |> P.map (\_ -> P.Done (List.reverse items))
                ]
        )


shoppingListSource : Parser ShoppingList.Source
shoppingListSource =
    P.succeed ShoppingList.Source
        |= stringWithoutCommas
        |. P.token ", "
        |= string
        |. P.token "\n"


stringWithoutCommas : Parser String
stringWithoutCommas =
    P.getChompedString <|
        P.chompWhile (\c -> c /= ',' && c /= '\n')


string : Parser String
string =
    P.getChompedString <|
        P.chompWhile (\c -> c /= '\n')


try : Parser a -> Parser (Maybe a)
try aParser =
    P.oneOf
        [ P.map Just aParser
        , P.succeed Nothing
        ]
