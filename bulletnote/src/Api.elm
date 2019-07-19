port module Api exposing (importItem, requestImportItem, saveItem)

import Item exposing (Item, itemDecoder, itemEncoder)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode


port saveItemPort : Encode.Value -> Cmd msg


saveItem : Item -> Cmd msg
saveItem item =
    saveItemPort <| itemEncoder item


port requestImportItemPort : Bool -> Cmd msg


requestImportItem : Cmd msg
requestImportItem =
    requestImportItemPort True


port importItemPort : (Decode.Value -> msg) -> Sub msg


importItem : (Maybe Item -> msg) -> Sub msg
importItem withItem =
    importItemPort (decodeValue itemDecoder >> Result.toMaybe >> withItem)
