module Shared exposing (Flags, RemoteData(..))


type alias Flags =
    { api : String
    }


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failure
