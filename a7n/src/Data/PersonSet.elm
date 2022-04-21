module Data.PersonSet exposing (PersonSet, add, beginRenaming, changeTempName, decoder, delete, encode, map, rename, updateMark)

import Calendar exposing (Date)
import Data.Mark as Mark exposing (Mark)
import Data.Person as Person exposing (Person)
import Dict
import Json.Decode as D
import Json.Encode as E


type alias PersonSet =
    List Person


map : (Person -> a) -> PersonSet -> List a
map =
    List.map


beginRenaming : Person -> PersonSet -> PersonSet
beginRenaming person =
    update { person | tempName = person.name }


changeTempName : Person -> String -> PersonSet -> PersonSet
changeTempName person tempName =
    update { person | tempName = tempName }


rename : Person -> PersonSet -> PersonSet
rename person =
    List.map
        (\p ->
            if p.name == person.name then
                Person.new person.tempName person.marks

            else
                p
        )
        >> List.sortBy .name


encode : PersonSet -> E.Value
encode =
    E.list Person.encode


decoder : D.Decoder PersonSet
decoder =
    D.list Person.decoder


add : String -> PersonSet -> PersonSet
add name people =
    Person.new name Dict.empty
        :: people
        |> List.sortBy .name


delete : Person -> PersonSet -> PersonSet
delete person =
    List.filter (\p -> p.name /= person.name)


update : Person -> PersonSet -> PersonSet
update person =
    List.map
        (\p ->
            if p.name == person.name then
                person

            else
                p
        )


updateMark : Person -> Date -> (Maybe Mark -> Maybe Mark) -> PersonSet -> PersonSet
updateMark person date updateFn =
    update { person | marks = Mark.update date updateFn person.marks }
