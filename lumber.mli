open Parser

type lumber = {
    date: date;
    note: string;
    tags: string list;
}

(*  node , l_tree, r_tree, height *)
type tree = 
    | Leaf
    | Node of lumber * tree * tree * int

val addLog: tree -> lumber -> tree