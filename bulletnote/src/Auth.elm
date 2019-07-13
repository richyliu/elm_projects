port module Auth exposing (login, logout)


port loginPort : () -> Cmd msg


login : Cmd msg
login =
    loginPort ()


port logoutPort : () -> Cmd msg


logout : Cmd msg
logout =
    logoutPort ()
