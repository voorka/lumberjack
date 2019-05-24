open Str
open Types
open Unix

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
    (string_of_int (d.tm_mon)) ^ "/" ^ (string_of_int d.tm_mday) ^ "/" ^ (string_of_int (d.tm_year))
    ^ " " ^ (string_of_int d.tm_hour) ^ ":" ^ (string_of_int d.tm_min) ^ ":" ^ (string_of_int d.tm_sec)

(* Takes in a string of the form DD/MM/YYYY HH:MM:SS and converts to date type*)
let extractDate d =
    let dlist = Str.split (Str.regexp ":\\|[\\/]+\\|[ \t]+") d in
    let dlist_int = List.map int_of_string dlist in
        match dlist_int with
        |m::d::y::h::mi::s::_ -> {tm_mon=m ; tm_mday=d; tm_year=y; tm_hour=h; tm_min=mi; tm_sec=s; tm_wday=0; tm_yday=0; tm_isdst=false}, false
        |m::d::y::_ -> {tm_mon=m; tm_mday=d; tm_year=y; tm_hour=0; tm_min=0; tm_sec =0;tm_wday=0;tm_yday=0;tm_isdst=false}, true
        |m::d::_ -> {tm_mon=m; tm_mday=d; tm_year=((localtime(gettimeofday())).tm_year); tm_hour=0; tm_min=0; tm_sec =0;tm_wday=0;tm_yday=0;tm_isdst=false}, true
    
(* Returns date and date a day later *)
let getRange (t:tm) : tm*tm=
    match t with
    |{tm_mon=m ; tm_mday=d; tm_year=y; tm_hour=0; tm_min=0; tm_sec=0;tm_wday=0;tm_yday=0;tm_isdst=false} -> (t,{tm_mon=m ; tm_mday=d+1; tm_year=y; tm_hour=0; tm_min=0; tm_sec =0;tm_wday=0;tm_yday=0;tm_isdst=false})

(* returns a date object of current time *)
let getDate:tm =
    localtime(gettimeofday())

(* Takes in string list, appends current date, and converts to lumber*)
let convert_to_new_lumber (note:string list) : lumber =
    let d = getDate in
    match note with
    | x -> {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (format_date d) x); tags=[]} 

(* Takes in string list with first string of the form DD/MM/YYYY HH:MM:SS and converts to lumber*)
let convert_to_lumber (note:string list) : lumber =
    match note with
    | h::t -> let d = fst (extractDate h) in {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (format_date d) t); tags=[]} 
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