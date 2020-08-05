open Parser
open Types
open Unix

(* Prints node option. For single notecase *)
let display_note_option note =
  match note with
  | Some x -> print_endline x.note
  | None -> print_endline "Did not find a note"

(* Might make a fancier later *)
let display_note (lumber : lumber) = print_endline lumber.note

(* Prints list of notes *)
let rec display_notes (note_list : lumber list) =
  match note_list with
  | h :: t ->
      display_note h;
      print_string "\n";
      display_notes t
  | _ -> print_endline "Did not find any more notes"

let print_metrics () : unit =
  let (date_freq : (tm * int) list) =
    Lumber.collect_metrics !Init.currentTreeref
  in
  let rec print_metric (date_freq : (tm * int) list) : unit =
    match date_freq with
    | (h, h1) :: t ->
        if h.tm_mon > 9 then (
          print_endline
            ( string_of_int h.tm_mon ^ "/" ^ string_of_int h.tm_year
            ^ " - characters: " ^ string_of_int h1 );
          print_metric t )
        else (
          print_endline
            ( string_of_int h.tm_mon ^ "/" ^ string_of_int h.tm_year
            ^ "  - characters: " ^ string_of_int h1 );
          print_metric t )
    | _ -> print_endline ""
  in
  print_metric date_freq
