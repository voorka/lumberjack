type date = {
    month: int;
    day: int;
    year: int;
    hour: int;
    minute: int;
    second: int;
}

type lumber = {
    date: date;
    note: string;
    tags: string list;
}

type comparison = EQ | LT | GT

(*  node , l_tree, r_tree, height *)
type tree = 
    | Leaf
    | Node of lumber * tree * tree * int


type verb = Get | Init

type target = Date of date | Obj of string

type command = {
    verb_word: verb;
    target: target;
}