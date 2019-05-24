open Parser
open Lumber
open Unix
open Init
open Types
open Arg

let display_note note =
    match note with
    |Some x -> print_endline("Found note: "); print_endline(x.note); 
    |None -> print_endline("Did not find a note")

let rec to_string arr i acc =
    if i == 1 then acc
    else to_string arr (i-1) (" " ^ arr.(i-1) ^ acc)

let rec to_list arr i acc =
    if i == 1 then acc
    else to_list arr (i-1) (arr.(i-1)::acc)

let print_note x =
    display_note (Lumber.getLog (Parser.extractDate (!(Parser.get_date_ref) ^":"^ x)) !Init.currentTreeref)

let main (args: string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    let speclist = [("-init", Arg.String Init.init, "Creates Entry Tree");
    ("--get-date", Arg.Tuple ([Arg.String Parser.set_date_ref; Arg.String print_note]), "Prints note from date to stdout. Date must be of form MM/DD/YYYY HH:MM:SS" );
    ]in
    let usage_msg = "Currently supported options include:" in
    Arg.parse speclist print_endline usage_msg

let () = main Sys.argv   

(* let main (args: string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    match Parser.parse (to_string args (Array.length args) "") with 
     |{verb_word = Init; target = Obj x} -> print_endline(x); Init.init x; 
     |{verb_word = Get; target = Date x} -> display_note (Lumber.getLog x !Init.currentTreeref)
     | _ -> failwith "Not implemented" *)

 (* ./main.byte foo.txt *)