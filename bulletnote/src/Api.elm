module Api exposing (addGirl, getAlbums, getDiy, getGirl, getGirls, getIdolAlbums, getIdolGirls, getIdolImages, getImages, getVideos, removeGirl)

import Album exposing (Album)
import Api.Endpoints exposing (graphqlRoot)
import Girl exposing (Girl)
import Http
import Id exposing (Id)
import IdolAlbum exposing (IdolAlbum)
import IdolGirl exposing (IdolGirl)
import IdolImage exposing (IdolImage)
import Image exposing (Image)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Url
import Video exposing (Video)



-- HTTP


baseGet : String -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
baseGet partialUrl partialDecoder toMsg =
    Http.get
        { url = buildUrl partialUrl
        , expect = Http.expectJson toMsg (wrapperDecoder partialDecoder)
        }


getGirls : (Result Http.Error (List Girl) -> msg) -> Cmd msg
getGirls =
    baseGet
        "{ models {id, name} }"
        Girl.girlsDecoder


addGirl : Girl -> (Result Http.Error String -> msg) -> Cmd msg
addGirl girl =
    baseGet
        ("{ addModel(id: \"" ++ Id.idToString girl.id ++ "\", name: \"" ++ girl.name ++ "\") }")
        (Decode.field "addModel" Decode.string)


removeGirl : Id -> (Result Http.Error String -> msg) -> Cmd msg
removeGirl id =
    baseGet
        ("{ removeModel(id: \"" ++ Id.idToString id ++ "\") }")
        (Decode.field "removeModel" Decode.string)


getGirl : Id -> (Result Http.Error Girl -> msg) -> Cmd msg
getGirl id =
    baseGet
        ("{ modelInfo(id: \"" ++ Id.idToString id ++ "\") {id, name} }")
        Girl.girlDecoder


getAlbums : Id -> (Result Http.Error (List Album) -> msg) -> Cmd msg
getAlbums id =
    baseGet
        ("{ model(id: \"" ++ Id.idToString id ++ "\") {album} }")
        Album.albumsDecoder


getImages : Id -> Album -> (Result Http.Error (List Image) -> msg) -> Cmd msg
getImages id album =
    baseGet
        ("{ album(id: \"" ++ Id.idToString id ++ "\", album: " ++ Album.albumToString album ++ ") {image} }")
        Image.imagesDecoder


getVideos : (Result Http.Error (List Video) -> msg) -> Cmd msg
getVideos =
    baseGet
        "{ videos }"
        Video.videosDecoder


getDiy : (Result Http.Error (List Image) -> msg) -> Cmd msg
getDiy =
    baseGet
        "{ diy }"
        Image.diyImagesDecoder


getIdolGirls : (Result Http.Error (List IdolGirl) -> msg) -> Cmd msg
getIdolGirls =
    baseGet
        "{ idolModels }"
        IdolGirl.idolGirlsDecoder


getIdolAlbums : IdolGirl -> Int -> (Result Http.Error (List IdolAlbum) -> msg) -> Cmd msg
getIdolAlbums idolGirl page =
    baseGet
        ("{ idolModel(id: \"" ++ IdolGirl.idolGirlToString idolGirl ++ "\", page: " ++ String.fromInt page ++ ") { id, cover } }")
        IdolAlbum.idolAlbumsDecoder


getIdolImages : IdolAlbum -> (Result Http.Error (List IdolImage) -> msg) -> Cmd msg
getIdolImages idolAlbum =
    baseGet
        ("{ idolAlbum(id: \"" ++ IdolAlbum.idolAlbumId idolAlbum ++ "\") { high, low } }")
        IdolImage.idolImagesDecoder



-- DECODERS


wrapperDecoder : Decoder a -> Decoder a
wrapperDecoder decoder =
    Decode.succeed identity
        |> required "data" decoder



-- PRIVATE


buildUrl : String -> String
buildUrl query =
    graphqlRoot ++ Url.percentEncode query
