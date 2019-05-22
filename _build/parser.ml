open Str

type verb = Get

type date = {
    month: int;
    day: int;
    year: int;
    hour: int;
    minute: int;
    second: int;
}
type target = Date of date | Obj of string

type command = {
    verb_word: verb;
    target: target;
}

let parse str =
    match Str.split (Str.regexp " ")(String.lowercase_ascii str) with
    |"get"::t -> {verb_word = Get; target = (Obj (String.concat " " t))}
    | x -> {verb_word = Get; target = (Obj (String.concat "" (x)))}