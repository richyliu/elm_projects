module Shared exposing (Status(..))


type Status a
    = Loading
    | Loaded a
    | Failed
