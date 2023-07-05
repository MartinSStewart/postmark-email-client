module Evergreen.V4.Postmark exposing (..)

import Evergreen.V4.Email.Html
import Evergreen.V4.EmailAddress
import List.Nonempty


type ApiKey
    = ApiKey String


type PostmarkEmailBody
    = BodyHtml Evergreen.V4.Email.Html.Html
    | BodyText String
    | BodyBoth Evergreen.V4.Email.Html.Html String


type alias PostmarkSendResponse =
    { errorCode : Int
    , message : String
    , to : List Evergreen.V4.EmailAddress.EmailAddress
    }


type SendEmailsError
    = UnknownError_
        { statusCode : Int
        , body : String
        }
    | PostmarkErrors (List.Nonempty.Nonempty PostmarkSendResponse)
    | NetworkError_
    | Timeout_
    | BadUrl_ String
