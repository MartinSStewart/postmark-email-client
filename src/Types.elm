module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Email.Html
import EmailAddress exposing (EmailAddress)
import Http
import Lamdera exposing (ClientId)
import List.Nonempty exposing (Nonempty)
import Postmark
import Serialize
import String.Nonempty exposing (NonemptyString)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
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


type SubmitStatus
    = NotSubmitted HasPressedSubmit (Result String ())
    | Submitting


type HasPressedSubmit
    = HasPressedSubmit
    | HasNotPressedSubmit


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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


type alias SaveData =
    { emailSubject : String
    , bodyText : String
    , bodyHtml : String
    , postmarkApiKey : String
    , emailTo : String
    , senderName : String
    , senderEmail : String
    }


type alias EmailRequest =
    { apiKey : Postmark.ApiKey
    , subject : NonemptyString
    , body : Postmark.PostmarkEmailBody
    , senderName : String
    , senderEmail : EmailAddress
    , emailTo : Nonempty EmailAddress
    }


type ToBackend
    = SendEmailRequest EmailRequest


type BackendMsg
    = SentEmail ClientId (Result Postmark.Error ())


type ToFrontend
    = SendEmailResponse (Result Postmark.Error ())
