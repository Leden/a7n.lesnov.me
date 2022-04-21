module Style exposing (button, deleteButton, theme)

import Css exposing (Color, hex, px, zero)
import Html.Styled as H exposing (Attribute, Html, styled)



---- THEME ----


type alias Theme =
    { primaryDark : Color
    , secondaryDark : Color
    , primaryLight : Color
    , secondaryLight : Color
    , accent : Color
    }


theme : Theme
theme =
    Theme
        (hex "252422")
        (hex "403D39")
        (hex "FFFCF2")
        (hex "CCC5B9")
        (hex "EB5E28")



---- ELEMENTS ----


basicButton : List (Attribute msg) -> List (Html msg) -> Html msg
basicButton =
    styled H.button
        [ Css.cursor Css.pointer
        , Css.backgroundColor Css.transparent
        , Css.borderWidth (px 1)
        , Css.borderStyle Css.solid
        , Css.margin3 zero (px 5) zero
        , Css.padding (px 3)
        ]


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    styled basicButton [ Css.borderColor theme.secondaryLight ]


deleteButton : List (Attribute msg) -> List (Html msg) -> Html msg
deleteButton =
    styled basicButton [ Css.borderColor Css.transparent ]
