module Api.Endpoints exposing (diyRoot, diyUrl, graphqlRoot, imageUrl, videoImageUrl, videoUrl)

import Album exposing (Album)
import Id exposing (Id)
import Image exposing (Image)
import Video exposing (Video)



-- PUBLIC


{-| Build a image url from the id, album number, and the image
-}
imageUrl : Id -> Album -> Image -> String
imageUrl id num image =
    imageRoot
        ++ String.join "/"
            [ Id.idToString id
            , Album.albumToString num
            , Image.imageToString image
            ]


{-| Build a video url from the video
-}
videoUrl : Video -> String
videoUrl video =
    videosRoot ++ Video.videoToString video


{-| Converts a video url to an image url by changing the ending to .jpg
-}
videoImageUrl : Video -> String
videoImageUrl video =
    videosRoot
        ++ (video
                |> Video.videoToString
                |> String.slice 0 -3
                |> (\prefix -> prefix ++ "jpg")
           )


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
