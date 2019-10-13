module ShoppingList exposing (Item, ShoppingList)


type alias ShoppingList =
    { items : List Item
    }


type alias Item =
    { name : String
    , quantity : String
    }
