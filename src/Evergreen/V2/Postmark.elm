module Evergreen.V2.Postmark exposing (..)

import Evergreen.V2.Email.Html


type ApiKey
    = ApiKey String


type PostmarkEmailBody
    = BodyHtml Evergreen.V2.Email.Html.Html
    | BodyText String
    | BodyBoth Evergreen.V2.Email.Html.Html String


type Error
    = UnknownError
        { statusCode : Int
        , body : String
        }
    | PostmarkError
        { errorCode : Int
        , message : String
        }
    | NetworkError
    | Timeout
    | BadUrl String
