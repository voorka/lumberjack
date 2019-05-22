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

let getMonth m =
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
    | _ -> raise (Failure "Not a valid month")

let formatDate d:string =
    (getMonth d.month) ^ " " ^ (string_of_int d.day) ^ ", " ^ (string_of_int (d.year+1900) )
    ^ " " ^ (string_of_int d.hour) ^ ":" ^ (string_of_int d.minute) ^ ":" ^ (string_of_int d.second)

(* returns a date object of current time *)
let getDate :date =
    let tm = localtime(gettimeofday()) in
    {month= tm.tm_mon; day = tm.tm_mday; year=tm.tm_year; hour=tm.tm_hour; minute=tm.tm_min; second = tm.tm_sec}

let convertToLumber note : lumber =
    let d = getDate in
    match note with
    | x -> {date=d; note=(List.fold_left (fun x y -> x ^ "\n" ^ y) (formatDate d) x); tags=[""]} 

let main (args: string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    match args.(1) with
    | txt -> (Pervasives.print_endline) (convertToLumber((read_lines txt))).note

(*  ./main.byte foo.txt *)
let () = main Sys.argv