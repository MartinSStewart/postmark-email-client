port module Ports exposing (..)

import Json.Decode
import Json.Encode


port got_local_storage_from_js : (Json.Decode.Value -> msg) -> Sub msg


port get_local_storage_to_js : () -> Cmd msg


port set_local_storage_to_js : Json.Encode.Value -> Cmd msg
