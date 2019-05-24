open Unix

type lumber = {
    date: tm;
    note: string;
    tags: string list;
}

type comparison = EQ | LT | GT

(*  node , l_tree, r_tree, height *)
type tree = 
    | Leaf
    | Node of lumber * tree * tree * int


type verb = Get | Init

type target = Date of tm | Obj of string

type command = {
    verb_word: verb;
    target: target;
}