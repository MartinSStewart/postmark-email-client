module Evergreen.V6.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import EmailAddress
import File
import Lamdera
import List.Nonempty
import Postmark
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
    , attachments :
        List
            { filename : String
            , mimeType : String
            , size : Int
            , content : Bytes.Bytes
            }
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
    | PressedAddAttachment
    | SelectedAttachments File.File (List File.File)
    | GotAttachmentContents File.File Bytes.Bytes
    | PressedRemoveAttachment String


type EmailBody
    = HtmlBody String
    | TextBody String
    | HtmlAndTextBody String String


type alias EmailRequest =
    { apiKey : Postmark.ApiKey
    , subject : String.Nonempty.NonemptyString
    , body : EmailBody
    , senderName : String
    , senderEmail : EmailAddress.EmailAddress
    , emailTo : List.Nonempty.Nonempty EmailAddress.EmailAddress
    , attachments :
        Dict.Dict
            String
            { mimeType : String
            , content : Bytes.Bytes
            }
    }


type ToBackend
    = SendEmailRequest EmailRequest


type BackendMsg
    = SentEmail Lamdera.ClientId (Result Postmark.SendEmailsError ())


type ToFrontend
    = SendEmailResponse (Result Postmark.SendEmailsError ())
