module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Element.Font
import Element.Input
import Email.Html
import Email.Html.Attributes
import EmailAddress exposing (EmailAddress)
import Html.Parser
import Http
import Lamdera
import List.Nonempty exposing (Nonempty)
import Ports
import Postmark
import Process
import Serialize exposing (Codec)
import String.Nonempty
import Task
import Toop exposing (T4(..), T5(..))
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
        , subscriptions =
            \m ->
                Ports.got_local_storage_from_js
                    (\json ->
                        Serialize.decodeFromJson
                            saveDataCodec
                            json
                            |> GotLocalStorage
                    )
        , view = view
        }


saveDataCodec : Codec e SaveData
saveDataCodec =
    Serialize.record SaveData
        |> Serialize.field .emailSubject Serialize.string
        |> Serialize.field .bodyText Serialize.string
        |> Serialize.field .bodyHtml Serialize.string
        |> Serialize.field .postmarkApiKey Serialize.string
        |> Serialize.field .emailTo Serialize.string
        |> Serialize.field .senderName Serialize.string
        |> Serialize.field .senderEmail Serialize.string
        |> Serialize.finishRecord


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , emailSubject = ""
      , bodyText = ""
      , bodyHtml = ""
      , postmarkApiKey = ""
      , emailTo = ""
      , senderName = ""
      , senderEmail = ""
      , submitStatus = NotSubmitted HasNotPressedSubmit (Ok ())
      , debounceCounter = 0
      }
    , Ports.get_local_storage_to_js ()
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
            case model.submitStatus of
                NotSubmitted _ _ ->
                    case validate model of
                        Ok emailRequest ->
                            ( { model | submitStatus = Submitting }
                            , Lamdera.sendToBackend (SendEmailRequest emailRequest)
                            )

                        Err error ->
                            ( { model | submitStatus = NotSubmitted HasPressedSubmit (Err error) }, Cmd.none )

                Submitting ->
                    ( model, Cmd.none )

        TypedBodyText text ->
            { model | bodyText = text } |> debounce

        TypedBodyHtml text ->
            { model | bodyHtml = text } |> debounce

        TypedApiKey text ->
            { model | postmarkApiKey = text } |> debounce

        TypedEmailSubject text ->
            { model | emailSubject = text } |> debounce

        TypedEmailTo text ->
            { model | emailTo = text } |> debounce

        TypedSenderName text ->
            { model | senderName = text } |> debounce

        TypedSenderEmail text ->
            { model | senderEmail = text } |> debounce

        GotLocalStorage result ->
            ( case result of
                Ok ok ->
                    { model
                        | emailSubject = ok.emailSubject
                        , bodyText = ok.bodyText
                        , bodyHtml = ok.bodyHtml
                        , postmarkApiKey = ok.postmarkApiKey
                        , emailTo = ok.emailTo
                        , senderName = ok.senderName
                        , senderEmail = ok.senderEmail
                        , submitStatus = NotSubmitted HasNotPressedSubmit (Ok ())
                    }

                Err _ ->
                    model
            , Cmd.none
            )

        CheckDebounce counter ->
            ( model
            , if counter == model.debounceCounter then
                Serialize.encodeToJson
                    saveDataCodec
                    { emailSubject = model.emailSubject
                    , bodyText = model.bodyText
                    , bodyHtml = model.bodyHtml
                    , postmarkApiKey = model.postmarkApiKey
                    , emailTo = model.emailTo
                    , senderName = model.senderName
                    , senderEmail = model.senderEmail
                    }
                    |> Ports.set_local_storage_to_js

              else
                Cmd.none
            )


debounce : FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
debounce model =
    ( { model | debounceCounter = model.debounceCounter + 1 }
    , Process.sleep 1000 |> Task.perform (\() -> CheckDebounce (model.debounceCounter + 1))
    )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SendEmailResponse result ->
            ( { model
                | submitStatus =
                    case model.submitStatus of
                        NotSubmitted _ _ ->
                            model.submitStatus

                        Submitting ->
                            NotSubmitted HasNotPressedSubmit (Result.mapError postmarkErrorToString result)
              }
            , Cmd.none
            )


postmarkErrorToString : Postmark.Error -> String
postmarkErrorToString error =
    case error of
        Postmark.UnknownError { statusCode, body } ->
            "Status code " ++ String.fromInt statusCode ++ ", error: " ++ body

        Postmark.PostmarkError { errorCode, message } ->
            "Postmark error, code " ++ String.fromInt errorCode ++ ", message: " ++ message

        Postmark.NetworkError ->
            "Network error"

        Postmark.Timeout ->
            "Request timed out"

        Postmark.BadUrl string ->
            "Bad url " ++ string


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Postmark email client"
    , body =
        [ Element.layout
            [ Element.padding 16
            , Element.newTabLink
                [ Element.Font.color (Element.rgb 0.2 0.3 1)
                , Element.alignBottom
                , Element.alignRight
                , Element.paddingXY 16 8
                ]
                { url = "https://github.com/MartinSStewart/postmark-email-client"
                , label = Element.text "View source code"
                }
                |> Element.inFront
            ]
            (Element.column
                [ Element.width (Element.maximum 800 Element.fill), Element.centerX, Element.spacing 24 ]
                [ Element.paragraph
                    [ Element.Font.size 18 ]
                    [ Element.text
                        "Use this app to send emails via the Postmark API. You can preview an email by sending it to your own email address. Changes you make are saved locally so it's safe to refresh the page."
                    ]
                , Ui.simpleTextInput
                    "Postmark API key"
                    (Just "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxx")
                    (case ( validateApiKey model.postmarkApiKey, model.submitStatus ) of
                        ( Err error, NotSubmitted HasPressedSubmit _ ) ->
                            Err error

                        _ ->
                            Ok ()
                    )
                    model.postmarkApiKey
                    TypedApiKey
                , Element.row
                    [ Element.width Element.fill, Element.spacing 16 ]
                    [ Ui.simpleTextInput "Sender name (optional)" Nothing (Ok ()) model.senderName TypedSenderName
                    , Ui.simpleTextInput "Sender email"
                        Nothing
                        (case ( validateEmail model.senderEmail, model.submitStatus ) of
                            ( Err error, NotSubmitted HasPressedSubmit _ ) ->
                                Err error

                            _ ->
                                Ok ()
                        )
                        model.senderEmail
                        TypedSenderEmail
                    ]
                , Ui.simpleTextInput
                    "List the people you want this survey emailed to. Comma separate each email."
                    (Just "johnz@example.com, bob@bob.com, jane123@mail.com")
                    (case ( validateEmails model.emailTo, model.submitStatus ) of
                        ( Err error, NotSubmitted HasPressedSubmit _ ) ->
                            Err error

                        _ ->
                            Ok ()
                    )
                    model.emailTo
                    TypedEmailTo
                , Ui.simpleTextInput
                    "Subject"
                    Nothing
                    (case ( validateSubject model.emailSubject, model.submitStatus ) of
                        ( Err error, NotSubmitted HasPressedSubmit _ ) ->
                            Err error

                        _ ->
                            Ok ()
                    )
                    model.emailSubject
                    TypedEmailSubject
                , Element.Input.multiline
                    [ Element.Font.size 16, Element.height (Element.minimum 80 Element.shrink) ]
                    { label = Element.Input.labelAbove [] (Element.text "Plain text body (the email can contain a plain text body, an html body, or both)")
                    , placeholder = Nothing
                    , text = model.bodyText
                    , onChange = TypedBodyText
                    , spellcheck = True
                    }
                , Element.Input.multiline
                    [ Element.Font.size 16, Element.height (Element.minimum 80 Element.shrink) ]
                    { label = Element.Input.labelAbove [] (Element.text "Html body")
                    , placeholder = Nothing
                    , text = model.bodyHtml
                    , onChange = TypedBodyHtml
                    , spellcheck = True
                    }
                , Element.column
                    [ Element.spacing 16, Element.width Element.fill ]
                    [ Ui.simpleButton PressedSubmit
                        (case model.submitStatus of
                            NotSubmitted _ _ ->
                                "Send email"

                            Submitting ->
                                "Sending..."
                        )
                    , Ui.errorText
                        (case model.submitStatus of
                            NotSubmitted _ result ->
                                result

                            Submitting ->
                                Ok ()
                        )
                    ]
                ]
            )
        ]
    }


validate : FrontendModel -> Result String EmailRequest
validate model =
    case
        T5
            (validateEmails model.emailTo)
            (validateEmail model.senderEmail)
            (validateSubject model.emailSubject)
            (validateApiKey model.postmarkApiKey)
            (validateHtmlBody model.bodyHtml)
    of
        T5 (Ok emailTo) (Ok senderEmail) (Ok subject) (Ok apiKey) (Ok bodyHtml) ->
            let
                bodyResult : Result String Postmark.PostmarkEmailBody
                bodyResult =
                    case ( String.trim model.bodyText, bodyHtml ) of
                        ( "", Nothing ) ->
                            Err "Plain text body and html body can't both be empty"

                        ( _, Nothing ) ->
                            Postmark.BodyText model.bodyText |> Ok

                        ( "", Just html ) ->
                            Postmark.BodyHtml html |> Ok

                        ( _, Just html ) ->
                            Postmark.BodyBoth html model.bodyText |> Ok
            in
            case bodyResult of
                Ok body ->
                    Ok
                        { apiKey = apiKey
                        , subject = subject
                        , body = body
                        , senderName = model.senderName
                        , senderEmail = senderEmail
                        , emailTo = emailTo
                        }

                Err err ->
                    Err err

        _ ->
            Err ""


validateHtmlBody : String -> Result String (Maybe Email.Html.Html)
validateHtmlBody text =
    case Html.Parser.run Html.Parser.noCharRefs text of
        Ok ok ->
            (case List.filterMap convertHtml ok of
                [] ->
                    Nothing

                [ single ] ->
                    Just single

                many ->
                    Email.Html.div [] many |> Just
            )
                |> Ok

        Err err ->
            Err "Invalid html"


convertHtml : Html.Parser.Node -> Maybe Email.Html.Html
convertHtml node =
    case node of
        Html.Parser.Text text ->
            Email.Html.text text |> Just

        Html.Parser.Comment _ ->
            Nothing

        Html.Parser.Element nodeName list nodes ->
            Email.Html.node nodeName (List.map convertAttribute list) (List.filterMap convertHtml nodes) |> Just


convertAttribute : ( String, String ) -> Email.Html.Attribute
convertAttribute ( name, value ) =
    Email.Html.Attributes.attribute name value


validateEmail : String -> Result String EmailAddress
validateEmail text =
    case EmailAddress.fromString text of
        Just a ->
            Ok a

        Nothing ->
            Err "Invalid email"


validateApiKey : String -> Result String Postmark.ApiKey
validateApiKey text =
    if String.isEmpty text then
        Err "API key required"

    else
        Ok (Postmark.apiKey text)


validateSubject : String -> Result String String.Nonempty.NonemptyString
validateSubject text =
    case String.Nonempty.fromString text of
        Just a ->
            Ok a

        Nothing ->
            Err "Subject can't be empty"


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
