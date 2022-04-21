module Main exposing (main)

import Browser
import Calendar
import CalendarSelect
import Css
    exposing
        ( Color
        , backgroundColor
        , border3
        , borderColor
        , borderStyle
        , borderWidth
        , center
        , color
        , fontFamily
        , height
        , hover
        , inherit
        , monospace
        , paddingLeft
        , px
        , solid
        , textAlign
        , transparent
        , width
        )
import Data.Mark as Mark
import Data.Person as Person exposing (Person)
import Data.PersonSet as PersonSet exposing (PersonSet)
import Html.Styled
    exposing
        ( Attribute
        , Html
        , div
        , form
        , input
        , map
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , toUnstyled
        , tr
        )
import Html.Styled.Attributes exposing (colspan, css, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import LocalStorage
import Ports.LocalStoragePort
import Style exposing (deleteButton, theme)
import Time



---- MODEL ----


type alias Model =
    { people : PersonSet
    , newName : String
    , calendarSelect : CalendarSelect.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( calendarSelect, cmd ) =
            CalendarSelect.init
    in
    ( { people = []
      , newName = ""
      , calendarSelect = calendarSelect
      }
    , Cmd.batch
        [ Ports.LocalStoragePort.getItem (localStoragePrefix ++ "/people")
        , cmd |> Cmd.map GotCalendarSelectMsg
        ]
    )



---- UPDATE ----


type Msg
    = LocalStorageOp LocalStorage.Response
    | SwitchMark Person Calendar.Date
    | ChangeNewName String
    | CreatePerson
    | DeletePerson Person
    | BeginRenamingPerson Person
    | ChangePersonTempName Person String
    | RenamePerson Person
    | GotCalendarSelectMsg CalendarSelect.Msg


localStoragePrefix : String
localStoragePrefix =
    "a7n.lesnov.me"


localStorage : LocalStorage.LocalStorage Msg
localStorage =
    LocalStorage.make
        Ports.LocalStoragePort.getItem
        Ports.LocalStoragePort.setItem
        Ports.LocalStoragePort.clear
        Ports.LocalStoragePort.listKeys
        localStoragePrefix


subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.responseHandler LocalStorageOp localStorage
        |> Ports.LocalStoragePort.response


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCalendarSelectMsg subMsg ->
            let
                ( calendarSelect, cmd ) =
                    CalendarSelect.update subMsg model.calendarSelect
            in
            ( { model | calendarSelect = calendarSelect }
            , cmd |> Cmd.map GotCalendarSelectMsg
            )

        SwitchMark person date ->
            let
                newPeople =
                    PersonSet.updateMark person date Mark.switch model.people

                newModel =
                    { model | people = newPeople }
            in
            ( newModel, persistModel newModel )

        LocalStorageOp response ->
            case response of
                LocalStorage.Item key value ->
                    ( tryLoadModel key value model, Cmd.none )

                LocalStorage.ItemNotFound _ ->
                    ( model, Cmd.none )

                LocalStorage.KeyList _ ->
                    ( model, Cmd.none )

                LocalStorage.Error _ ->
                    ( model, Cmd.none )

        ChangeNewName newName ->
            ( { model | newName = newName }, Cmd.none )

        CreatePerson ->
            let
                newPeople =
                    if String.isEmpty model.newName then
                        model.people

                    else
                        PersonSet.add model.newName model.people

                newModel =
                    { model | people = newPeople, newName = "" }
            in
            ( newModel
            , Cmd.batch [ persistModel newModel ]
            )

        DeletePerson person ->
            let
                newPeople =
                    PersonSet.delete person model.people

                newModel =
                    { model | people = newPeople }
            in
            ( newModel
            , Cmd.batch [ persistModel newModel ]
            )

        BeginRenamingPerson person ->
            let
                newModel =
                    { model | people = PersonSet.beginRenaming person model.people }
            in
            ( newModel, Cmd.none )

        RenamePerson person ->
            let
                newModel =
                    { model | people = PersonSet.rename person model.people }
            in
            ( newModel, persistModel newModel )

        ChangePersonTempName person tempName ->
            let
                newModel =
                    { model | people = PersonSet.changeTempName person tempName model.people }
            in
            ( newModel, Cmd.none )


persistModel : Model -> Cmd Msg
persistModel model =
    Cmd.batch
        [ Ports.LocalStoragePort.setItem
            ( localStoragePrefix ++ "/people"
            , PersonSet.encode model.people
            )
        ]


tryLoadModel : String -> E.Value -> Model -> Model
tryLoadModel key value model =
    case key of
        "people" ->
            case D.decodeValue PersonSet.decoder value of
                Err _ ->
                    model

                Ok people ->
                    { model | people = people }

        _ ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    viewTable model.people model.calendarSelect model.newName


viewTable : PersonSet -> CalendarSelect.Model -> String -> Html Msg
viewTable people calendarSelect newName =
    table
        [ css
            [ backgroundColor theme.primaryLight
            , color theme.secondaryDark
            , fontFamily monospace
            ]
        ]
        [ thead []
            [ tr []
                (emptyNodes 2 th
                    ++ [ th [ colspan (CalendarSelect.countDates calendarSelect) ]
                            [ CalendarSelect.view calendarSelect
                                |> map GotCalendarSelectMsg
                            ]
                       ]
                )
            , tr []
                (emptyNodes 2 th
                    ++ CalendarSelect.mapDates viewDayHeaderCell calendarSelect
                )
            ]
        , tbody [] <|
            PersonSet.map (viewPersonRow calendarSelect) people
                ++ [ tr []
                        [ td [] []
                        , td []
                            [ createPersonDisplay newName ]
                        ]
                   ]
        ]


emptyNodes : Int -> (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> List (Html Msg)
emptyNodes count node =
    node [] [ text "" ]
        |> List.repeat count


createPersonDisplay : String -> Html Msg
createPersonDisplay newName =
    form [ onSubmit CreatePerson ]
        [ input [ value newName, onInput ChangeNewName ] [] ]


viewDayHeaderCell : Calendar.Date -> Calendar.Date -> Html Msg
viewDayHeaderCell currentDate date =
    th
        [ css
            [ borderColor
                (if date == currentDate then
                    theme.secondaryLight

                 else
                    theme.primaryLight
                )
            , borderWidth (px 1)
            , borderStyle solid
            , color (date |> Calendar.getWeekday |> weekdayColor)
            ]
        ]
        [ div []
            [ date
                |> Calendar.getWeekday
                |> weekdayShort
                |> text
            ]
        , div []
            [ text (date |> Calendar.getDay |> String.fromInt)
            ]
        ]


weekdayColor : Time.Weekday -> Color
weekdayColor weekday =
    case weekday of
        Time.Sat ->
            theme.secondaryLight

        Time.Sun ->
            theme.secondaryLight

        _ ->
            theme.secondaryDark


weekdayShort : Time.Weekday -> String
weekdayShort weekday =
    case weekday of
        Time.Mon ->
            "Mo"

        Time.Tue ->
            "Tu"

        Time.Wed ->
            "We"

        Time.Thu ->
            "Th"

        Time.Fri ->
            "Fr"

        Time.Sat ->
            "Sa"

        Time.Sun ->
            "Su"


viewPersonRow : CalendarSelect.Model -> Person -> Html Msg
viewPersonRow calendarSelect person =
    tr
        [ css
            [ hover [ borderColor theme.secondaryLight ]
            , border3 (px 1) solid transparent
            ]
        ]
        (viewPersonDeleteBtnCell person
            :: viewPersonNameCell person
            :: CalendarSelect.mapDates (viewMarkCell person) calendarSelect
        )


viewPersonNameCell : Person -> Html Msg
viewPersonNameCell person =
    td
        [ onDoubleClick (BeginRenamingPerson person)
        , css
            [ paddingLeft (px 10)
            , borderStyle solid
            , borderWidth (px 1)
            , borderColor inherit
            ]
        ]
        [ if Person.isRenaming person then
            form [ onSubmit (RenamePerson person) ]
                [ input
                    [ value person.tempName
                    , onInput <| ChangePersonTempName person
                    ]
                    []
                ]

          else
            text person.name
        ]


viewPersonDeleteBtnCell : Person -> Html Msg
viewPersonDeleteBtnCell person =
    td []
        [ deleteButton
            [ onClick (DeletePerson person)
            ]
            [ text "✖" ]
        ]


viewMarkCell : Person -> Calendar.Date -> Calendar.Date -> Html Msg
viewMarkCell person currentDate date =
    let
        markStr =
            person |> Person.getMark date |> Mark.view
    in
    td
        [ onClick (SwitchMark person date)
        , css
            [ if date == currentDate then
                borderColor theme.secondaryLight

              else
                borderColor inherit
            , borderStyle solid
            , borderWidth (px 1)
            , width (px 20)
            , height (px 20)
            , textAlign center
            ]
        ]
        [ text markStr ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
