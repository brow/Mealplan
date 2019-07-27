module Serialize exposing (recipeFromString)

import Parser as P exposing ((|.), (|=), Parser)
import Recipe exposing (Recipe)
import Result


recipeFromString : String -> Result String Recipe
recipeFromString =
    P.run recipe >> Result.mapError Debug.toString


recipe : Parser Recipe
recipe =
    P.succeed Recipe
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
        |= string
        |. P.token ", "
        |= string
        |= (try <|
                P.succeed identity
                    |. P.token ", "
                    |= string
           )
        |. P.token "\n"


string : Parser String
string =
    P.getChompedString <|
        P.chompWhile (\c -> c /= ',' && c /= '\n')


try : Parser a -> Parser (Maybe a)
try aParser =
    P.oneOf
        [ P.map Just aParser
        , P.succeed Nothing
        ]
