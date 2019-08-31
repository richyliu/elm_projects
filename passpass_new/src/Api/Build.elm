port module Api.Build exposing (getBuild, receiveBuild)


port getBuildPort : () -> Cmd msg


{-| Tell JS to get the build string
-}
getBuild : Cmd msg
getBuild =
    getBuildPort ()


port receiveBuildPort : (String -> msg) -> Sub msg


{-| Receive the build string from JS
-}
receiveBuild : (String -> msg) -> Sub msg
receiveBuild toMsg =
    receiveBuildPort toMsg
