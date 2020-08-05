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
      print_endline "\n";
      display_notes t
  | _ -> print_endline "Did not find any more notes"

let get_range_dates (beginDate : tm) (endDate : tm) =
  Lumber.get_range_logs beginDate endDate !Init.currentTreeref

(* Prints notes in range beginDate to endDate  *)
let print_range_dates (beginDate : tm) (endDate : tm) =
  display_notes (get_range_dates beginDate endDate)

(* Prints range of notes from string input of form MM/DD/YYYY//HH:MM:SS-MM/DD/YYYY//HH:MM:SS *)
let print_range_note x =
  let dates = Str.split (Str.regexp "-") x in

  match dates with
  | [ h; t ] ->
      let date_first_resp = Parser.extractDate h in
      let date_second_resp = Parser.extractDate t in
      let date_first = fst date_first_resp in
      let date_second = fst date_second_resp in
      if snd date_first_resp then
        let date_first = fst (getRange date_first) in
        if snd date_second_resp then
          let date_second = snd (getRange date_second) in
          print_range_dates date_first date_second
  | _ ->
      print_endline
        "Date range must be of form MM/DD/YYYY//HH:MM:SS-MM/DD/YYYY//HH:MM:SS"

(* Prints notes from input string. If only specifies to the day, prints entire day *)
let print_note x =
  let d = Parser.extractDate x in
  if snd d then
    match Parser.getRange (fst d) with
    | h, t -> print_range_dates (h : tm) (t : tm)
  else
    display_note_option
      (Lumber.get_log (fst (Parser.extractDate x)) !Init.currentTreeref)

let find_occurences x =
  display_notes (List.rev (Lumber.find_all_notes x !Init.currentTreeref))

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

let print_count keyword =
  print_endline
    ( keyword ^ " occurs "
    ^ string_of_int
        (List.length (Lumber.find_all_notes keyword !Init.currentTreeref))
    ^ " times" )
