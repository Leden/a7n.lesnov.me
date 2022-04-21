module Data.Mark exposing (Mark, MarkSet, decoder, encode, switch, update, view)

import Calendar
import Dict
import Json.Decode as D
import Json.Encode as E



---- MODEL ----


type Mark
    = Spoken
    | Absent


type alias MarkSet =
    Dict.Dict Int Mark


decoder : D.Decoder MarkSet
decoder =
    D.keyValuePairs D.string
        |> D.map (List.map (Tuple.mapBoth String.toInt fromString))
        |> D.map (List.filterMap pullMaybe)
        |> D.map Dict.fromList


encode : MarkSet -> E.Value
encode =
    Dict.toList
        >> List.map (Tuple.mapBoth String.fromInt (toString >> E.string))
        >> E.object


pullMaybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
pullMaybe pair =
    case pair of
        ( Just a, Just b ) ->
            Just ( a, b )

        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing


toString : Mark -> String
toString mark =
    case mark of
        Spoken ->
            "Spoken"

        Absent ->
            "Absent"


fromString : String -> Maybe Mark
fromString mark =
    case mark of
        "Spoken" ->
            Just Spoken

        "Absent" ->
            Just Absent

        _ ->
            Nothing


view : Maybe Mark -> String
view maybeMark =
    case maybeMark of
        Just Absent ->
            "×"

        Just Spoken ->
            "∙"

        Nothing ->
            " "


update : Calendar.Date -> (Maybe Mark -> Maybe Mark) -> MarkSet -> MarkSet
update date =
    date
        |> Calendar.toMillis
        |> Dict.update


switch : Maybe Mark -> Maybe Mark
switch mark =
    case mark of
        Nothing ->
            Just Spoken

        Just Spoken ->
            Just Absent

        Just Absent ->
            Nothing
