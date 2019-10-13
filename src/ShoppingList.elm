module ShoppingList exposing (Item, ShoppingList, Source)


type alias ShoppingList =
    { items : List Item
    }


type alias Item =
    { name : String
    , quantity : String
    }


type alias Source =
    { name : String
    , url : String
    }
