module Api.Endpoints exposing (diyRoot, diyUrl, graphqlRoot, imageRoot, origin, videosRoot)

{-| Build a diy image url from the diy image
-}


diyUrl : Image -> String
diyUrl image =
    diyRoot ++ Image.imageToString image


{-| Graphql base to add the URI encoded query to
-}
graphqlRoot : String
graphqlRoot =
    origin ++ ":4000/graphql?query="



-- PRIVATE


imageRoot : String
imageRoot =
    origin ++ "/images/metart/"


videosRoot : String
videosRoot =
    origin ++ "/videos/"


diyRoot : String
diyRoot =
    "https://rliu.me/diy/"


origin : String
origin =
    "http://10.0.1.38"
