module Evergreen.V1.Postmark exposing (..)

import Evergreen.V1.Email.Html


type ApiKey
    = ApiKey String


type PostmarkEmailBody
    = BodyHtml Evergreen.V1.Email.Html.Html
    | BodyText String
    | BodyBoth Evergreen.V1.Email.Html.Html String


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
