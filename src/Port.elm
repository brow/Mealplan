port module Port exposing (error, urlToOpen)


port error : String -> Cmd msg


port urlToOpen : String -> Cmd msg
