module CalendarSelect exposing
    ( Model
    , Msg
    , countDates
    , init
    , mapDates
    , update
    , view
    )

import Calendar exposing (Date)
import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Style exposing (button)
import Task
import Time
import Time.Format.I18n.I_en_us



---- MODEL ----


type alias Model =
    Maybe
        { currentDate : Date
        , selectedDate : Date
        }


init : ( Model, Cmd Msg )
init =
    ( Nothing
    , fetchCurrentTime
    )


fetchCurrentTime : Cmd Msg
fetchCurrentTime =
    Task.perform Initialize Time.now



---- UPDATE ----


type Msg
    = Initialize Time.Posix
    | SelectNextMonth
    | SelectPrevMonth
    | ResetToCurrent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize posix ->
            let
                date =
                    Calendar.fromPosix posix
            in
            ( Just
                { currentDate = date
                , selectedDate = date
                }
            , Cmd.none
            )

        SelectNextMonth ->
            ( updateSelectedDate Calendar.incrementMonth model, Cmd.none )

        SelectPrevMonth ->
            ( updateSelectedDate Calendar.decrementMonth model, Cmd.none )

        ResetToCurrent ->
            ( model, fetchCurrentTime )


updateSelectedDate : (Date -> Date) -> Model -> Model
updateSelectedDate fn =
    Maybe.map
        (\r ->
            { r | selectedDate = fn r.selectedDate }
        )



---- TRANSFORM ----


getDates : Model -> List Date
getDates =
    Maybe.map (.selectedDate >> Calendar.getDatesInMonth)
        >> Maybe.withDefault []


mapDates : (Date -> Date -> a) -> Model -> List a
mapDates fn =
    Maybe.map
        (\{ currentDate, selectedDate } ->
            selectedDate
                |> Calendar.getDatesInMonth
                |> List.map (fn currentDate)
        )
        >> Maybe.withDefault []


countDates : Model -> Int
countDates =
    getDates >> List.length


getYearMonth : Date -> ( Int, Int )
getYearMonth date =
    ( date |> Calendar.getYear
    , date |> Calendar.getMonth |> Calendar.monthToInt
    )


compareMonthYear : Date -> Date -> Order
compareMonthYear d1 d2 =
    compare (getYearMonth d1) (getYearMonth d2)



---- VIEW ----


view : Model -> Html Msg
view model =
    H.div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            ]
        ]
        [ H.span []
            [ prevMonthBtn model
            , leftTodayBtn model
            ]
        , maybeHtml (.selectedDate >> monthYearDisplay) model
        , H.span []
            [ rightTodayBtn model
            , nextMonthBtn model
            ]
        ]


prevMonthBtn : Model -> Html Msg
prevMonthBtn model =
    let
        text =
            model
                |> maybeHtml
                    (\{ selectedDate } ->
                        selectedDate |> Calendar.decrementMonth |> monthText
                    )
    in
    button [ onClick SelectPrevMonth ] [ H.text "← ", text ]


nextMonthBtn : Model -> Html Msg
nextMonthBtn model =
    let
        text =
            model
                |> maybeHtml
                    (\{ selectedDate } ->
                        selectedDate |> Calendar.incrementMonth |> monthText
                    )
    in
    button [ onClick SelectNextMonth ] [ text, H.text " →" ]


leftTodayBtn : Model -> Html Msg
leftTodayBtn =
    maybeHtml
        (\{ currentDate, selectedDate } ->
            if compareMonthYear selectedDate currentDate == GT then
                button [ onClick ResetToCurrent ]
                    [ H.text "↞ Now: ", monthText currentDate ]

            else
                H.text ""
        )


rightTodayBtn : Model -> Html Msg
rightTodayBtn =
    maybeHtml
        (\{ currentDate, selectedDate } ->
            if compareMonthYear selectedDate currentDate == LT then
                button [ onClick ResetToCurrent ]
                    [ H.text "Now: ", monthText currentDate, H.text "↠" ]

            else
                H.text ""
        )


maybeHtml : (a -> Html Msg) -> Maybe a -> Html Msg
maybeHtml fn =
    Maybe.map fn >> Maybe.withDefault (H.text "")


monthYearDisplay : Date -> Html Msg
monthYearDisplay date =
    H.span []
        [ monthText date
        , H.text " "
        , yearText date
        ]


monthText : Date -> Html Msg
monthText =
    Calendar.getMonth
        >> Time.Format.I18n.I_en_us.monthName
        >> H.text


yearText : Date -> Html Msg
yearText =
    Calendar.getYear >> String.fromInt >> H.text
