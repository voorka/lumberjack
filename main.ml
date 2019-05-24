open Parser
open Lumber
open Unix
open Init
open Types
open Arg

let display_note_option note =
    match note with
    |Some x -> print_endline("Found note: "); print_endline(x.note); 
    |None -> print_endline("Did not find a note")

let rec display_note note_list =
    match note_list with
    |h::t -> print_endline("Found note: "); print_endline(h.note); display_note t
    | _ -> print_endline("Did not find any notes")

let print_range_dates (beginDate:date) (endDate:date) = 
    display_note (Lumber.getRangeLogs beginDate endDate !Init.currentTreeref)

let print_range_note x =
    let dates = Str.split (Str.regexp "-") x in
    match dates with
        |h::t::[] -> print_range_dates (fst (Parser.extractDate h)) (fst (Parser.extractDate t))
        | _ -> print_endline("Date range must be of form MM/DD/YYYY//HH:MM:SS-MM/DD/YYYY//HH:MM:SS")

let print_note x =
    let d = Parser.extractDate x in
        if (snd d) then begin
                match (Parser.getRange (fst d)) with
                | h::t::[] -> print_range_dates (h:date) (t:date)
                end
        else display_note_option (Lumber.getLog (fst (Parser.extractDate x)) !Init.currentTreeref)

let main (args: string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    let speclist = [("-init", Arg.String Init.init, "Creates Entry Tree");
    ("--get-date",  Arg.String print_note, "Prints note from date to stdout. Date must be of form MM/DD/YYYY//HH:MM:SS" );
    ("--get-dates", Arg.String print_range_note, "Prints note from date range to stdout. Date range must be of form MM/DD/YYYY//HH:MM:SS-MM/DD/YYYY//HH:MM:SS" );
    ]in
    let usage_msg = "Currently supported options include:" in
    Arg.parse speclist print_endline usage_msg

let () = main Sys.argv   