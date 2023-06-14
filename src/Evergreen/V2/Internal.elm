module Evergreen.V2.Internal exposing (..)

import Bytes


type Attribute
    = StyleAttribute String String
    | Attribute String String


type ImageType
    = Jpeg
    | Png
    | Gif


type Html
    = Node String (List Attribute) (List Html)
    | InlineImage
        { content : Bytes.Bytes
        , imageType : ImageType
        }
        (List Attribute)
        (List Html)
    | TextNode String
