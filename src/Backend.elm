module Backend exposing (..)

import Dict
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
            ( model
            , Postmark.sendEmails
                (SentEmail clientId)
                emailRequest.apiKey
                (List.Nonempty.map
                    (\emailTo ->
                        { from = { name = emailRequest.senderName, email = emailRequest.senderEmail }
                        , to = Nonempty { name = "", email = emailTo } []
                        , subject = emailRequest.subject
                        , body = emailRequest.body
                        , messageStream = Postmark.BroadcastEmail
                        , attachments = Dict.empty
                        }
                            |> Postmark.addAttachments emailRequest.attachments
                    )
                    emailRequest.emailTo
                )
            )
