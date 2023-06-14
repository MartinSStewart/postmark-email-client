module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty
import Postmark
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
            , Postmark.sendEmail
                (SentEmail clientId)
                emailRequest.apiKey
                { from = { name = emailRequest.senderName, email = emailRequest.senderEmail }
                , to = List.Nonempty.map (\email -> { name = "", email = email }) emailRequest.emailTo
                , subject = emailRequest.subject
                , body = emailRequest.body
                , messageStream = "broadcast"
                }
            )
