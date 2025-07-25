module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Email.Html
import Email.Html.Attributes
import EmailAddress exposing (EmailAddress)
import File
import File.Select
import Html
import Html.Attributes
import Html.Lazy
import Html.Parser
import Http
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
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


type SaveDataVersions
    = SaveDataV1_ SaveDataV1
    | SaveDataV2_ SaveData


saveDataCodec : Codec e SaveData
saveDataCodec =
    Serialize.customType
        (\a b value ->
            case value of
                SaveDataV1_ data ->
                    a data

                SaveDataV2_ data ->
                    b data
        )
        |> Serialize.variant1 SaveDataV1_ saveDataCodecV1
        |> Serialize.variant1 SaveDataV2_ saveDataCodecV2
        |> Serialize.finishCustomType
        |> Serialize.map
            (\value ->
                case value of
                    SaveDataV1_ data ->
                        { emailSubject = data.emailSubject
                        , bodyText = data.bodyText
                        , bodyHtml = data.bodyHtml
                        , postmarkApiKey = data.postmarkApiKey
                        , emailTo = data.emailTo
                        , senderName = data.senderName
                        , senderEmail = data.senderEmail
                        , derivePlainTextFromHtml = False
                        }

                    SaveDataV2_ data ->
                        data
            )
            SaveDataV2_


saveDataCodecV1 : Codec e SaveDataV1
saveDataCodecV1 =
    Serialize.record SaveDataV1
        |> Serialize.field .emailSubject Serialize.string
        |> Serialize.field .bodyText Serialize.string
        |> Serialize.field .bodyHtml Serialize.string
        |> Serialize.field .postmarkApiKey Serialize.string
        |> Serialize.field .emailTo Serialize.string
        |> Serialize.field .senderName Serialize.string
        |> Serialize.field .senderEmail Serialize.string
        |> Serialize.finishRecord


saveDataCodecV2 : Codec e SaveData
saveDataCodecV2 =
    Serialize.record SaveData
        |> Serialize.field .emailSubject Serialize.string
        |> Serialize.field .bodyText Serialize.string
        |> Serialize.field .bodyHtml Serialize.string
        |> Serialize.field .postmarkApiKey Serialize.string
        |> Serialize.field .emailTo Serialize.string
        |> Serialize.field .senderName Serialize.string
        |> Serialize.field .senderEmail Serialize.string
        |> Serialize.field .derivePlainTextFromHtml Serialize.bool
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
      , submitStatus = NotSubmitted HasNotPressedSubmit Nothing
      , debounceCounter = 0
      , derivePlainTextFromHtml = False
      , attachments = []
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
                            ( { model | submitStatus = NotSubmitted HasPressedSubmit (Just (Err error)) }, Cmd.none )

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
                        , derivePlainTextFromHtml = ok.derivePlainTextFromHtml
                        , submitStatus = NotSubmitted HasNotPressedSubmit Nothing
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
                    , derivePlainTextFromHtml = model.derivePlainTextFromHtml
                    }
                    |> Ports.set_local_storage_to_js

              else
                Cmd.none
            )

        PressedDerivePlainTextFromHtml isEnabled ->
            { model | derivePlainTextFromHtml = isEnabled } |> debounce

        PressedAddAttachment ->
            ( model, File.Select.files [ "*/*" ] SelectedAttachments )

        SelectedAttachments file files ->
            ( model
            , List.map
                (\file2 -> File.toBytes file2 |> Task.perform (GotAttachmentContents file2))
                (file :: files)
                |> Cmd.batch
            )

        GotAttachmentContents file bytes ->
            ( { model
                | attachments =
                    { filename = File.name file
                    , mimeType = File.mime file
                    , size = File.size file
                    , content = bytes
                    }
                        :: model.attachments
              }
            , Cmd.none
            )

        PressedRemoveAttachment filename ->
            ( { model
                | attachments =
                    List.filter (\attachment -> attachment.filename /= filename) model.attachments
              }
            , Cmd.none
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
                            NotSubmitted HasNotPressedSubmit (Just (Result.mapError postmarkErrorToString result))
              }
            , Cmd.none
            )


postmarkErrorToString : Postmark.SendEmailsError -> String
postmarkErrorToString error =
    case error of
        Postmark.UnknownError_ { statusCode, body } ->
            "Status code " ++ String.fromInt statusCode ++ ", error: " ++ body

        Postmark.PostmarkErrors nonempty ->
            List.Nonempty.toList nonempty
                |> List.map
                    (\{ errorCode, message, to } ->
                        case to of
                            [] ->
                                "Email failed. " ++ message

                            _ ->
                                "Email to "
                                    ++ (List.map EmailAddress.toString to |> String.join ", ")
                                    ++ " failed. "
                                    ++ message
                    )
                |> String.join "\n"

        Postmark.NetworkError_ ->
            "Network error"

        Postmark.Timeout_ ->
            "Request timed out"

        Postmark.BadUrl_ string ->
            "Bad url " ++ string


derivePlainTextFromHtml : String -> String
derivePlainTextFromHtml htmlText =
    case Html.Parser.run Html.Parser.noCharRefs htmlText of
        Ok html ->
            List.map derivePlainTextFromHtmlHelper html |> String.concat

        Err _ ->
            ""


derivePlainTextFromHtmlHelper : Html.Parser.Node -> String
derivePlainTextFromHtmlHelper html =
    case html of
        Html.Parser.Text text ->
            text

        Html.Parser.Comment _ ->
            ""

        Html.Parser.Element tagName attributes nodes ->
            String.concat (List.map derivePlainTextFromHtmlHelper nodes)
                ++ (if tagName == "a" then
                        List.findMap
                            (\( name, value ) ->
                                if name == "href" then
                                    " " ++ value |> Just

                                else
                                    Nothing
                            )
                            attributes
                            |> Maybe.withDefault ""

                    else if tagName == "br" then
                        "\n"

                    else
                        ""
                   )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Postmark email client"
    , body =
        [ Element.layout
            [ Element.paddingEach { left = 16, right = 16, top = 16, bottom = 48 }
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
                [ Element.width (Element.maximum 1000 Element.fill)
                , Element.centerX
                , Element.spacing 24
                ]
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
                    { label = Element.Input.labelAbove [ Element.Font.bold ] (Element.text "Html body")
                    , placeholder = Nothing
                    , text = model.bodyHtml
                    , onChange = TypedBodyHtml
                    , spellcheck = True
                    }
                , Element.Input.checkbox
                    [ Element.width Element.fill, Element.Font.size 16, Element.Font.bold ]
                    { onChange = PressedDerivePlainTextFromHtml
                    , icon = Element.Input.defaultCheckbox
                    , checked = model.derivePlainTextFromHtml
                    , label =
                        Element.Input.labelRight
                            [ Element.width Element.fill ]
                            (Element.paragraph [] [ Element.text "Derive plain text body from html" ])
                    }
                , Element.Input.multiline
                    [ Element.Font.size 16
                    , Element.height (Element.minimum 80 Element.shrink)
                    , if model.derivePlainTextFromHtml then
                        Element.Background.color (Element.rgb 0.9 0.9 0.9)

                      else
                        Element.Background.color (Element.rgb 1 1 1)
                    , if model.derivePlainTextFromHtml then
                        Element.Font.color (Element.rgb 0.5 0.5 0.5)

                      else
                        Element.Font.color (Element.rgb 0 0 0)
                    ]
                    { label =
                        Element.Input.labelAbove
                            [ Element.Font.bold
                            , if model.derivePlainTextFromHtml then
                                Element.alpha 0.4

                              else
                                Element.alpha 1
                            ]
                            (Element.text "Plain text body (the email can contain a plain text body, an html body, or both)")
                    , placeholder = Nothing
                    , text = model.bodyText
                    , onChange = TypedBodyText
                    , spellcheck = True
                    }
                , Element.column
                    [ Element.spacing 8 ]
                    [ Ui.button
                        [ Element.Border.rounded 8
                        , Element.Background.color (Element.rgb 0.4 0.4 0.4)
                        , Element.paddingXY 20 8
                        , Element.Font.size 16
                        , Element.Font.bold
                        ]
                        PressedAddAttachment
                        (Element.el [ Element.Font.color (Element.rgb 1 1 1), Element.centerX ] (Element.text "Add attachments"))
                    , Element.column
                        [ Element.Font.size 16, Element.spacing 2 ]
                        (List.map
                            (\attachment ->
                                Element.row
                                    [ Element.spacing 16, Element.width Element.fill ]
                                    [ Element.text attachment.filename
                                    , Element.row
                                        [ Element.spacing 8, Element.alignRight ]
                                        [ Element.el
                                            [ Element.Font.color (Element.rgb255 130 130 130) ]
                                            (Element.text (" " ++ (toFloat attachment.size / 1024 |> ceiling |> String.fromInt) ++ "kb"))
                                        , Ui.button
                                            [ Element.Background.color (Element.rgb255 220 38 38)
                                            , Element.Font.color (Element.rgb 1 1 1)
                                            , Element.paddingXY 8 4
                                            ]
                                            (PressedRemoveAttachment attachment.filename)
                                            (Element.text "×")
                                        ]
                                    ]
                            )
                            model.attachments
                        )
                    ]
                , Element.row
                    [ Element.spacing 16, Element.width Element.fill, Element.Font.size 16 ]
                    [ Element.column
                        [ Element.width Element.fill, Element.spacing 8, Element.alignTop ]
                        [ Element.paragraph
                            [ Element.Font.bold ]
                            [ Element.text "Html preview*" ]
                        , Html.Lazy.lazy previewHtml model.bodyHtml
                            |> Element.html
                            |> Element.el
                                [ Element.width Element.fill
                                , Element.Background.color (Element.rgb 0.95 0.95 0.95)
                                , Element.padding 8
                                , Html.Attributes.style "white-space" "normal" |> Element.htmlAttribute
                                ]
                        ]
                    , Element.column
                        [ Element.width Element.fill, Element.spacing 8, Element.alignTop ]
                        [ Element.el
                            [ Element.Font.bold ]
                            (Element.text "Plain text preview")
                        , Html.div
                            [ Html.Attributes.style "white-space" "pre-wrap" ]
                            [ Html.text
                                (if model.derivePlainTextFromHtml then
                                    derivePlainTextFromHtml model.bodyHtml

                                 else
                                    model.bodyText
                                )
                            ]
                            |> Element.html
                            |> Element.el
                                [ Element.width Element.fill
                                , Element.Background.color (Element.rgb 0.95 0.95 0.95)
                                , Element.padding 8
                                ]
                        ]
                    ]
                , Element.paragraph
                    [ Element.Font.size 16 ]
                    [ Element.text "*Html preview is only an approximation. The actual email shown to the recipient will vary depending on what email client is used." ]
                , Element.row
                    [ Element.spacing 16, Element.width Element.fill ]
                    [ Ui.simpleButton PressedSubmit
                        (case model.submitStatus of
                            NotSubmitted _ _ ->
                                "Send email"

                            Submitting ->
                                "Sending..."
                        )
                    , case model.submitStatus of
                        NotSubmitted _ (Just result) ->
                            case result of
                                Ok () ->
                                    Element.paragraph
                                        [ Element.Font.color (Element.rgb 0 0.6 0) ]
                                        [ Element.text "Email sent successfully!" ]

                                Err _ ->
                                    Ui.errorText result

                        _ ->
                            Element.none
                    ]
                ]
            )
        ]
    }


previewHtml : String -> Html.Html msg
previewHtml htmlText =
    case validateHtmlBody htmlText of
        Ok (Just html) ->
            Email.Html.toHtml html

        Ok Nothing ->
            Html.text ""

        Err error ->
            Html.text error


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
                bodyText : String
                bodyText =
                    if model.derivePlainTextFromHtml then
                        derivePlainTextFromHtml model.bodyHtml

                    else
                        String.trim model.bodyText

                bodyResult : Result String EmailBody
                bodyResult =
                    case ( bodyText, bodyHtml ) of
                        ( "", Nothing ) ->
                            Err "Plain text body and html body can't both be empty"

                        ( _, Nothing ) ->
                            TextBody model.bodyText |> Ok

                        ( "", Just _ ) ->
                            HtmlBody model.bodyHtml |> Ok

                        ( _, Just _ ) ->
                            HtmlAndTextBody model.bodyHtml model.bodyText |> Ok
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
                        , attachments =
                            List.map
                                (\attachment ->
                                    ( attachment.filename
                                    , { mimeType = attachment.mimeType
                                      , content = attachment.content
                                      }
                                    )
                                )
                                model.attachments
                                |> Dict.fromList
                        }

                Err err ->
                    Err err

        _ ->
            Err ""


validateHtmlBody : String -> Result String (Maybe Email.Html.Html)
validateHtmlBody text =
    case String.replace "\u{000D}" "" text |> String.replace "\n" "<br>" |> Html.Parser.run Html.Parser.noCharRefs of
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
