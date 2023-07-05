module Evergreen.V4.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V4.EmailAddress
import Evergreen.V4.Postmark
import Lamdera
import List.Nonempty
import Serialize
import String.Nonempty
import Url


type HasPressedSubmit
    = HasPressedSubmit
    | HasNotPressedSubmit


type SubmitStatus
    = NotSubmitted HasPressedSubmit (Maybe (Result String ()))
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
    , derivePlainTextFromHtml : Bool
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
    , derivePlainTextFromHtml : Bool
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
    | PressedDerivePlainTextFromHtml Bool


type alias EmailRequest =
    { apiKey : Evergreen.V4.Postmark.ApiKey
    , subject : String.Nonempty.NonemptyString
    , body : Evergreen.V4.Postmark.PostmarkEmailBody
    , senderName : String
    , senderEmail : Evergreen.V4.EmailAddress.EmailAddress
    , emailTo : List.Nonempty.Nonempty Evergreen.V4.EmailAddress.EmailAddress
    }


type ToBackend
    = SendEmailRequest EmailRequest


type BackendMsg
    = SentEmail Lamdera.ClientId (Result Evergreen.V4.Postmark.SendEmailsError ())


type ToFrontend
    = SendEmailResponse (Result Evergreen.V4.Postmark.SendEmailsError ())
