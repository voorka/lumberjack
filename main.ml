open Parser
open Lumber
open Unix

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

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

let format_date d:string =
    (string_of_int (d.month)) ^ "/" ^ (string_of_int d.day) ^ "/" ^ (string_of_int (d.year))
    ^ " " ^ (string_of_int d.hour) ^ ":" ^ (string_of_int d.minute) ^ ":" ^ (string_of_int d.second)

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

(* let convert_to_lumber (note:string list) : lumber =
    let d = getDate in
    match note with
    | x -> {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (format_date d) x); tags=[]}  *)

let convert_to_lumber (note:string list) : lumber =
    match note with
    | h::t -> let d = extractDate h in {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (format_date d) t); tags=[]} 
    | _ -> raise (Failure "Empty note in convert to lumber")

(* Takes in string list and returns lumber list *)
let rec process_string_list (slst:string list) (acc:string list) (llst:lumber list): lumber list= 
    match slst with
    | ""::t -> process_string_list t [] ((convert_to_lumber acc)::llst )
    | x::t -> process_string_list t (acc@[x]) llst
    | _ -> if acc == [] then llst else (convert_to_lumber acc)::llst 

let main (args: string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    match args.(1) with
    | txt -> (List.iter (fun x -> Pervasives.print_endline (x.note ^ "\n")) (List.rev(process_string_list (read_lines txt) [] [])))

(*  ./main.byte foo.txt *)
let () = main Sys.argv