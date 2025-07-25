module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Email.Html
import EmailAddress exposing (EmailAddress)
import File exposing (File)
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
    , derivePlainTextFromHtml : Bool
    , attachments : List { filename : String, mimeType : String, size : Int, content : Bytes }
    }


type SubmitStatus
    = NotSubmitted HasPressedSubmit (Maybe (Result String ()))
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
    | PressedDerivePlainTextFromHtml Bool
    | PressedAddAttachment
    | SelectedAttachments File (List File)
    | GotAttachmentContents File Bytes
    | PressedRemoveAttachment String


type alias SaveDataV1 =
    { emailSubject : String
    , bodyText : String
    , bodyHtml : String
    , postmarkApiKey : String
    , emailTo : String
    , senderName : String
    , senderEmail : String
    }


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


type alias EmailRequest =
    { apiKey : Postmark.ApiKey
    , subject : NonemptyString
    , body : Postmark.EmailBody
    , senderName : String
    , senderEmail : EmailAddress
    , emailTo : Nonempty EmailAddress
    , attachments : Dict String { mimeType : String, content : Bytes }
    }


type ToBackend
    = SendEmailRequest EmailRequest


type BackendMsg
    = SentEmail ClientId (Result Postmark.SendEmailsError ())


type ToFrontend
    = SendEmailResponse (Result Postmark.SendEmailsError ())
