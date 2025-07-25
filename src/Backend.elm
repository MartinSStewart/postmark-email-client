module Backend exposing (..)

import Dict
import Email.Html
import Frontend
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty exposing (Nonempty(..))
import Postmark
import Task
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( {}, Cmd.none )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        SentEmail clientId result ->
            ( model, Lamdera.sendToFrontend clientId (SendEmailResponse result) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        SendEmailRequest emailRequest ->
            let
                attachments : Postmark.Attachments
                attachments =
                    Postmark.attachments emailRequest.attachments

                body =
                    case emailRequest.body of
                        HtmlBody html ->
                            case Frontend.validateHtmlBody html of
                                Ok (Just html2) ->
                                    Postmark.HtmlBody html2

                                _ ->
                                    Postmark.HtmlBody (Email.Html.text "Something went wrong")

                        TextBody string ->
                            Postmark.TextBody string

                        HtmlAndTextBody html string ->
                            Postmark.HtmlAndTextBody
                                (case Frontend.validateHtmlBody html of
                                    Ok (Just html2) ->
                                        html2

                                    _ ->
                                        Email.Html.text "Something went wrong"
                                )
                                string
            in
            ( model
            , Postmark.sendEmails
                (SentEmail clientId)
                emailRequest.apiKey
                (List.Nonempty.map
                    (\emailTo ->
                        { from = { name = emailRequest.senderName, email = emailRequest.senderEmail }
                        , to = Nonempty { name = "", email = emailTo } []
                        , subject = emailRequest.subject
                        , body = body
                        , messageStream = Postmark.BroadcastEmail
                        , attachments = attachments
                        }
                    )
                    emailRequest.emailTo
                )
            )
