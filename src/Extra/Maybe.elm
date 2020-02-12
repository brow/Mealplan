module Extra.Maybe exposing (values, when)


values : List (Maybe a) -> List a
values =
    List.filterMap identity


when : Bool -> a -> Maybe a
when condition value =
    if condition then
        Just value

    else
        Nothing
