module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.EmailAddress
import Evergreen.V1.Postmark
import Lamdera
import List.Nonempty
import Serialize
import String.Nonempty
import Url


type HasPressedSubmit
    = HasPressedSubmit
    | HasNotPressedSubmit


type SubmitStatus
    = NotSubmitted HasPressedSubmit (Result String ())
    | Submitting


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , emailSubject : String
    , bodyText : String
    , bodyHtml : String
    , postmarkApiKey : String
    , emailTo : String
    , senderName : String
    , senderEmail : String
    , submitStatus : SubmitStatus
    , debounceCounter : Int
    }


type alias BackendModel =
    {}


type alias SaveData =
    { emailSubject : String
    , bodyText : String
    , bodyHtml : String
    , postmarkApiKey : String
    , emailTo : String
    , senderName : String
    , senderEmail : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedSubmit
    | TypedEmailSubject String
    | TypedBodyText String
    | TypedBodyHtml String
    | TypedApiKey String
    | TypedEmailTo String
    | TypedSenderName String
    | TypedSenderEmail String
    | GotLocalStorage (Result (Serialize.Error ()) SaveData)
    | CheckDebounce Int


type alias EmailRequest =
    { apiKey : Evergreen.V1.Postmark.ApiKey
    , subject : String.Nonempty.NonemptyString
    , body : Evergreen.V1.Postmark.PostmarkEmailBody
    , senderName : String
    , senderEmail : Evergreen.V1.EmailAddress.EmailAddress
    , emailTo : List.Nonempty.Nonempty Evergreen.V1.EmailAddress.EmailAddress
    }


type ToBackend
    = SendEmailRequest EmailRequest


type BackendMsg
    = SentEmail Lamdera.ClientId (Result Evergreen.V1.Postmark.Error ())


type ToFrontend
    = SendEmailResponse (Result Evergreen.V1.Postmark.Error ())
