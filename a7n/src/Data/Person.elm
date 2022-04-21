module Data.Person exposing (Person, decoder, encode, getMark, isRenaming, new)

import Calendar exposing (Date)
import Data.Mark as Mark exposing (Mark, MarkSet)
import Dict
import Json.Decode as D
import Json.Encode as E


type alias Person =
    { name : String
    , tempName : String

    -- Dict key is Date as milliseconds
    , marks : MarkSet
    }


new : String -> MarkSet -> Person
new name marks =
    { name = name, tempName = "", marks = marks }


encode : Person -> E.Value
encode { name, marks } =
    E.object
        [ ( "name", E.string name )
        , ( "marks", Mark.encode marks )
        ]


decoder : D.Decoder Person
decoder =
    D.map2 new
        (D.field "name" D.string)
        (D.field "marks" Mark.decoder)


getMark : Date -> Person -> Maybe Mark
getMark date person =
    Dict.get (Calendar.toMillis date) person.marks


isRenaming : Person -> Bool
isRenaming { tempName } =
    not (String.isEmpty tempName)
