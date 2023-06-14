module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import EmailAddress exposing (EmailAddress)
import Html
import Html.Attributes as Attr
import Lamdera
import List.Nonempty exposing (Nonempty)
import Types exposing (..)
import Ui
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , emailSubject = ""
      , emailBody = ""
      , postmarkApiKey = ""
      , emailTo = ""
      , senderName = ""
      , senderEmail = ""
      , submitStatus = NotSubmitted
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PressedSubmit ->
            ( { model | submitStatus = Submitting }, Cmd.none )

        TypedEmailBody text ->
            ( { model | emailBody = text }, Cmd.none )

        TypedApiKey text ->
            ( { model | postmarkApiKey = text }, Cmd.none )

        TypedEmailSubject text ->
            ( { model | emailSubject = text }, Cmd.none )

        TypedEmailTo text ->
            ( { model | emailTo = text }, Cmd.none )

        TypedSenderName text ->
            ( { model | senderName = text }, Cmd.none )

        TypedSenderEmail text ->
            ( { model | senderEmail = text }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SendEmailResponse ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            []
            (Element.column
                [ Element.width Element.fill, Element.spacing 24 ]
                [ Ui.simpleTextInput "Postmark API key" Nothing (Ok ()) model.postmarkApiKey TypedApiKey
                , Element.row
                    [ Element.width Element.fill, Element.spacing 16 ]
                    [ Ui.simpleTextInput "Sender name" Nothing (Ok ()) model.senderName TypedSenderName
                    , Ui.simpleTextInput "Sender email" Nothing (Ok ()) model.senderName TypedSenderEmail
                    ]
                , Ui.simpleTextInput
                    "List the people you want this survey emailed to. Comma separate each email."
                    (Just "johnz@example.com, bob@bob.com, jane123@mail.com")
                    (Ok ())
                    model.emailTo
                    TypedApiKey
                , Ui.simpleTextInput "Subject" Nothing (Ok ()) model.emailSubject TypedEmailSubject
                , Ui.simpleTextInput "Body" Nothing (Ok ()) model.emailBody TypedEmailBody
                , Ui.simpleButton PressedSubmit "Send email"
                ]
            )
        ]
    }


validateEmails : String -> Result String (Nonempty EmailAddress)
validateEmails text =
    let
        emails : List ( String, Maybe EmailAddress )
        emails =
            String.split "," text
                |> List.filterMap
                    (\subtext ->
                        let
                            trimmed =
                                String.trim subtext
                        in
                        if trimmed == "" then
                            Nothing

                        else
                            Just ( trimmed, EmailAddress.fromString trimmed )
                    )

        validEmails : List EmailAddress
        validEmails =
            List.filterMap Tuple.second emails

        invalidEmails : List String
        invalidEmails =
            List.filterMap
                (\( subtext, maybeValid ) ->
                    if maybeValid == Nothing then
                        Just subtext

                    else
                        Nothing
                )
                emails
    in
    case invalidEmails of
        [] ->
            case List.Nonempty.fromList validEmails of
                Just nonempty ->
                    Ok nonempty

                Nothing ->
                    Err "Include at least one email address. Otherwise the survey won't be sent to anyone!"

        [ invalidEmail ] ->
            invalidEmail ++ " is not a valid email" |> Err

        _ ->
            invalidEmails
                |> String.join ", "
                |> (\a -> a ++ " are not valid emails")
                |> Err
