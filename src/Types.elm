module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import EmailAddress exposing (EmailAddress)
import Http
import Postmark
import String.Nonempty exposing (NonemptyString)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , emailSubject : String
    , emailBody : String
    , postmarkApiKey : String
    , emailTo : String
    , senderName : String
    , senderEmail : String
    , submitStatus : SubmitStatus
    }


type SubmitStatus
    = NotSubmitted
    | Submitting


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedSubmit
    | TypedEmailSubject String
    | TypedEmailBody String
    | TypedApiKey String
    | TypedEmailTo String
    | TypedSenderName String
    | TypedSenderEmail String


type alias EmailRequest =
    { apiKey : Postmark.ApiKey
    , subject : NonemptyString
    , body : String
    , senderName : String
    , senderEmail : EmailAddress
    }


type ToBackend
    = SendEmailRequest EmailRequest


type BackendMsg
    = SentEmail (Result Http.Error ())


type ToFrontend
    = SendEmailResponse
