open Str
open Types
open Unix

let date_ref = ref ""

(* Takes in a file name and returns the lines in that file *)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let format_date d:string =
    (string_of_int (d.month)) ^ "/" ^ (string_of_int d.day) ^ "/" ^ (string_of_int (d.year))
    ^ " " ^ (string_of_int d.hour) ^ ":" ^ (string_of_int d.minute) ^ ":" ^ (string_of_int d.second)

(* Takes in a string of the form DD/MM/YYYY HH:MM:SS and converts to date type*)
let extractDate d:date =
    let dlist = Str.split (Str.regexp ":\\|/\\|[ \t]+") d in
    let dlist_int = List.map int_of_string dlist in
        match dlist_int with
        |m::d::y::h::mi::s::_ ->
            {month=m ; day=d; year=y; hour=h; minute=mi; second =s}

(* returns a date object of current time *)
let getDate:date =
    let tm = localtime(gettimeofday()) in
    {month= tm.tm_mon; day = tm.tm_mday; year=tm.tm_year; hour=tm.tm_hour; minute=tm.tm_min; second = tm.tm_sec}

let set_date_ref (s:string):unit =
    date_ref := s

let get_date_ref : string ref =
    date_ref

(* Takes in string list, appends current date, and converts to lumber*)
let convert_to_new_lumber (note:string list) : lumber =
    let d = getDate in
    match note with
    | x -> {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (format_date d) x); tags=[]} 

(* Takes in string list with first string of the form DD/MM/YYYY HH:MM:SS and converts to lumber*)
let convert_to_lumber (note:string list) : lumber =
    match note with
    | h::t -> let d = extractDate h in {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (format_date d) t); tags=[]} 
    | _ -> raise (Failure "Empty note in convert to lumber")


(* Takes in string list and returns lumber list. Assumes list consists of string lists 
seprated by empty lines that begin with a line of the form DD/MM/YYYY HH:MM:SS *)
let rec process_string_list (slst:string list) (acc:string list) (llst:lumber list): lumber list= 
    match slst with
    | ""::t -> process_string_list t [] ((convert_to_lumber acc)::llst )
    | x::t -> process_string_list t (acc@[x]) llst
    | _ -> if acc == [] then llst else (convert_to_lumber acc)::llst 


let txtToLumberList txt =
    (List.rev(process_string_list (read_lines txt) [] []))

let parse str =
    print_endline(str);
    match Str.split (Str.regexp " ")(String.lowercase_ascii str) with
    |"get"::t -> print_endline(format_date (extractDate (String.concat " " t)));{verb_word = Get; target = (Date (extractDate (String.concat " " t)))} 
    |"init"::t -> {verb_word = Init; target = (Obj (String.concat " " t))}
    | x -> {verb_word = Get; target = (Obj (String.concat "" (x)))}

(* let get_month m =
    match m with
    | 0 -> "January"
    | 1 -> "February"
    | 2 -> "March"
    | 3 -> "April"
    | 4 -> "May" 
    | 5 -> "June"
    | 6 -> "July"
    | 7 -> "August"
    | 8 -> "September"
    | 9 -> "October"
    | 10 -> "November"
    | 11 -> "December"
    | _ -> raise (Failure "Not a valid month") *)
(* 
let format_date d:string =
    (string_of_int (d.month+1)) ^ "/" ^ (string_of_int d.day) ^ "/" ^ (string_of_int (d.year+1900) )
    ^ " " ^ (string_of_int d.hour) ^ ":" ^ (string_of_int d.minute) ^ ":" ^ (string_of_int d.second) *)