module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Album exposing (Album)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Id exposing (Id)
import IdolAlbum exposing (IdolAlbum)
import IdolGirl exposing (IdolGirl)
import Image exposing (Image)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)



-- ROUTING


type Route
    = Root
    | Home
    | Gallery Id
    | Album Id Album (Maybe Image)
    | Shuffle Int
    | Login
    | Videos Int
    | Diy Int
    | Settings
    | IdolHome
    | IdolGallery IdolGirl Int
    | IdolAlbum IdolGirl Int IdolAlbum


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root Parser.top
        , Parser.map Home (s "home")
        , Parser.map Gallery (s "gallery" </> Id.idParser)
        , Parser.map Album (s "album" </> Id.idParser </> Album.albumParser <?> Image.imageQueryParser)
        , Parser.map Shuffle (s "shuffle" </> Parser.int)
        , Parser.map Login (s "login")
        , Parser.map Videos (s "videos" </> Parser.int)
        , Parser.map Diy (s "diy" </> Parser.int)
        , Parser.map Settings (s "settings")
        , Parser.map IdolHome (s "idol")
        , Parser.map IdolGallery (s "idol" </> IdolGirl.idolGirlParser </> Parser.int)
        , Parser.map IdolAlbum (s "idol" </> IdolGirl.idolGirlParser </> Parser.int </> IdolAlbum.idolAlbumParser)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Root ->
                    []

                Home ->
                    [ "home" ]

                Gallery id ->
                    [ "gallery", Id.idToString id ]

                Album id album maybeImage ->
                    [ "album"
                    , Id.idToString id
                    , Album.albumToString album
                    ]
                        ++ (case maybeImage of
                                Just image ->
                                    [ "?image=" ++ Image.imageToString image ]

                                Nothing ->
                                    []
                           )

                Shuffle seed ->
                    [ "shuffle", String.fromInt seed ]

                Login ->
                    [ "login" ]

                Videos seed ->
                    [ "videos", String.fromInt seed ]

                Diy seed ->
                    [ "diy", String.fromInt seed ]

                Settings ->
                    [ "settings" ]

                IdolHome ->
                    [ "idol" ]

                IdolGallery idolGirl page ->
                    [ "idol", IdolGirl.idolGirlToUrlString idolGirl, String.fromInt page ]

                IdolAlbum idolGirl page idolAlbum ->
                    [ "idol"
                    , IdolGirl.idolGirlToUrlString idolGirl
                    , String.fromInt page
                    , IdolAlbum.idolAlbumToUrlString idolAlbum
                    ]
    in
    "/" ++ String.join "/" pieces
