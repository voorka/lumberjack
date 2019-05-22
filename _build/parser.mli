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