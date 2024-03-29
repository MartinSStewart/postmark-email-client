module Ui exposing (button, errorText, simpleButton, simpleTextInput)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input


button : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
button attributes onPress label =
    Element.Input.button attributes { onPress = Just onPress, label = label }


simpleButton : msg -> String -> Element msg
simpleButton onPress text =
    button
        [ Element.Border.rounded 8
        , Element.Background.color (Element.rgb 0.2 0.4 0.2)
        , Element.paddingXY 40 8
        , Element.Font.bold
        ]
        onPress
        (Element.el [ Element.Font.color (Element.rgb 1 1 1), Element.centerX ] (Element.text text))


simpleTextInput : String -> Maybe String -> Result String ok -> String -> (String -> msg) -> Element msg
simpleTextInput label placeholder errorText2 text onChange =
    Element.column
        [ Element.spacing 4, Element.width Element.fill, Element.alignTop ]
        [ Element.Input.text
            [ Element.Font.size 16 ]
            { text = text
            , onChange = onChange
            , label = Element.Input.labelAbove [] (Element.paragraph [ Element.Font.bold ] [ Element.text label ])
            , placeholder = Maybe.map (\placeholder2 -> Element.Input.placeholder [] (Element.text placeholder2)) placeholder
            }
        , errorText errorText2
        ]


errorText : Result String a -> Element msg
errorText text =
    case text of
        Err error ->
            String.split "\n" error
                |> List.map (\subtext -> Element.paragraph [] [ Element.text subtext ])
                |> Element.column
                    [ Element.spacing 6
                    , Element.Font.color (Element.rgb 1 0 0)
                    , Element.Font.size 16
                    , Element.width Element.fill
                    ]

        Ok _ ->
            Element.none
