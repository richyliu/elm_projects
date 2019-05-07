module Session exposing (Session, changes, navKey, newGuest)

import Browser.Navigation as Nav


type Session
    = LoggedIn Nav.Key
    | Guest Nav.Key


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key ->
            key

        Guest key ->
            key


newGuest : Nav.Key -> Session
newGuest =
    Guest



-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    -- TODO: implement
    Sub.none
