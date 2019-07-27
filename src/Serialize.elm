module Serialize exposing (recipeFromString)

import Parser as P exposing ((|.), (|=), Parser)
import Recipe exposing (Recipe)
import Result


recipeFromString : String -> Result String Recipe
recipeFromString =
    P.run parser >> Result.mapError Debug.toString


parser : Parser Recipe
parser =
    P.map Recipe <|
        P.loop []
            (\ingredients ->
                P.oneOf
                    [ P.end
                        |> P.map (\_ -> P.Done (List.reverse ingredients))
                    , ingredientParser
                        |> P.map (\i -> P.Loop (i :: ingredients))
                    ]
            )


ingredientParser : Parser Recipe.Ingredient
ingredientParser =
    P.succeed Recipe.Ingredient
        |= ingredientPartParser
        |. P.token ", "
        |= ingredientPartParser
        |= (try <|
                P.succeed identity
                    |. P.token ", "
                    |= ingredientPartParser
           )
        |. try (P.token "\n")


ingredientPartParser : Parser String
ingredientPartParser =
    P.getChompedString <|
        P.chompWhile (\c -> c /= ',' && c /= '\n')


try : Parser a -> Parser (Maybe a)
try aParser =
    P.oneOf
        [ P.map Just aParser
        , P.succeed Nothing
        ]
