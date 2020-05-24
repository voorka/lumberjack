open Parser
open Lumber
open Unix
open Init
open Types
open Arg

(* Prints node option. For single notecase *)
let display_note_option note =
  match note with
  | Some x -> print_endline x.note
  | None -> print_endline "Did not find a note"

(* Prints list of notes *)
let rec display_note (note_list : lumber list) =
  match note_list with
  | h :: t ->
      print_endline (h.note ^ "\n");
      display_note t
  | _ -> print_endline "Did not find any more notes"

let get_range_dates (beginDate : tm) (endDate : tm) =
  Lumber.get_range_logs beginDate endDate !Init.currentTreeref

(* Prints notes in range beginDate to endDate  *)
let print_range_dates (beginDate : tm) (endDate : tm) =
  display_note (get_range_dates beginDate endDate)

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

let get_all () =
  print_endline
    ( List.length
        (get_range_dates
           (Lumber.get_earliest_date !Init.currentTreeref)
           Parser.getDate)
    |> string_of_int )

(* Prints all notes from 0/0 to today *)
let print_all () =
  print_range_dates
    (Lumber.get_earliest_date !Init.currentTreeref)
    Parser.getDate

let find_occurences x =
  display_note (List.rev (Lumber.find_all_notes x !Init.currentTreeref))

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

let templates =
  [ "What did you learn today?"; "What is your favorite song today?" ]

let make_lumber note =
  let d : tm = getDate in
  let note_with_metadata = "\n" ^ format_date d ^ "\n" ^ note in
  write_lines !Init.file_ref [ note_with_metadata ];
  let tree = !Init.currentTreeref in
  let new_lum = { date = d; note = note_with_metadata; tags = [] } in
  Init.currentTreeref := Lumber.add_log tree new_lum

let make_note () =
  let note = input_line Pervasives.stdin in
  make_lumber note

let gen_rand () =
  Random.self_init ();
  let template = List.nth templates (List.length templates |> Random.int) in
  print_endline template;
  let resp = input_line Pervasives.stdin in
  let note : string = template ^ "\n" ^ resp in
  make_lumber note

let main (args : string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    let speclist =
      [
        ("-i", Arg.String Init.init, "Creates Entry Tree");
        ( "-gd",
          Arg.String print_note,
          "Prints note from date to stdout. Date must be of form \
           MM/DD/YYYY//HH:MM:SS" );
        ( "-gds",
          Arg.String print_range_note,
          "Prints note from date range to stdout. Date range must be of form \
           MM/DD/YYYY//HH:MM:SS-MM/DD/YYYY//HH:MM:SS" );
        ("-ga", Arg.Unit print_all, "Prints all notes");
        ( "-find",
          Arg.String find_occurences,
          "Finds all notes containing keyword" );
        ( "-metrics",
          Arg.Unit print_metrics,
          "Prints character count metrics from past months" );
        ("-nc", Arg.Unit get_all, "Prints the total number of notes");
        ( "-fc",
          Arg.String print_count,
          "Prints the number of notes containing keyword" );
        ("-r", Arg.Unit gen_rand, "Generate a random note template");
        ("-n", Arg.Unit make_note, "Write a new note");
      ]
    in
    let usage_msg = "Currently supported options include:" in
    Arg.parse speclist print_endline usage_msg

let () = main Sys.argv
