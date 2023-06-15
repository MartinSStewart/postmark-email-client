module Evergreen.V3.Postmark exposing (..)

import Evergreen.V3.Email.Html
import Evergreen.V3.EmailAddress
import List.Nonempty


type ApiKey
    = ApiKey String


type PostmarkEmailBody
    = BodyHtml Evergreen.V3.Email.Html.Html
    | BodyText String
    | BodyBoth Evergreen.V3.Email.Html.Html String


type alias PostmarkSendResponse =
    { errorCode : Int
    , message : String
    , to : List Evergreen.V3.EmailAddress.EmailAddress
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
