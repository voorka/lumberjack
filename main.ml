open Unix
open Init
open Types
open Arg
open Print
open Parser

let inorder (funs : 'a -> unit) =
  let rec inorder_aux tree (funs : 'a -> unit) =
    match !Init.currentTreeref with
    | Leaf -> ()
    | Node (d, l, r, _) ->
        inorder_aux l funs;
        funs d;
        inorder_aux r funs
  in
  inorder_aux !Init.currentTreeref funs

let inorder_acc acc (funs : 'a -> 'b -> 'a) =
  let rec inorder_acc_aux tree acc (funs : 'a -> 'b -> 'a) =
    match !Init.currentTreeref with
    | Leaf -> acc
    | Node (d, l, r, _) ->
        let acc = inorder_acc_aux l acc funs in
        let acc = funs acc d in
        inorder_acc_aux r acc funs
  in
  inorder_acc_aux !Init.currentTreeref acc funs

(* Write tree to a file *)
let write_tree () =
  let oc = open_out !Init.file_ref in
  (fun x -> output_string oc (x.note ^ "\n\n")) |> inorder;
  close_out oc

let make_lumber note =
  let d : tm = getDate in
  let note_with_metadata = "\n" ^ format_date d ^ "\n" ^ note in
  let tree = !Init.currentTreeref in
  let new_log = { date = d; note = note_with_metadata; tags = [] } in
  Init.currentTreeref := Lumber.add_log tree new_log;
  write_tree ()

let rec read_input acc =
  let note = input_line Pervasives.stdin in
  if String.length note < 1 then List.rev acc |> String.concat "\n"
  else read_input (note :: acc)

let make_note () = read_input [] |> make_lumber

let print_tree () = (fun x -> print_string (x.note ^ "\n\n")) |> inorder

let node_count () =
  let acc = ref 0 in
  (fun _ -> acc := !acc + 1) |> inorder;
  print_int !acc;
  print_string " logs \n"

let find_logs_with_keyword keyword : lumber list =
  (fun acc x -> Lumber.find keyword acc x) |> inorder_acc []

let find_logs x = find_logs_with_keyword x |> display_notes

let print_count keyword =
  find_logs_with_keyword keyword |> List.length |> print_int;
  print_string "\n"

let templates =
  [
    "What did you learn today?";
    "What is your favorite song today?";
    "One thing I would tell myself from a year ago is:";
  ]

let gratitude_journaling =
  [
    "Who are you grateful for today?"; "What in your life are you grateful for?";
  ]

let gen_prompt prompts =
  Random.self_init ();
  let template = List.nth prompts (List.length prompts |> Random.int) in
  print_endline template;
  let resp = input_line Pervasives.stdin in
  let note : string = template ^ "\n" ^ resp in
  make_lumber note

let gen_rand () = gen_prompt templates

let gen_gratitude () = gen_prompt gratitude_journaling

let amend () =
  let tree = !Init.currentTreeref in
  let last_log : lumber = Lumber.get_last_log tree in
  Print.display_note last_log;
  let addendum = read_input [ last_log.note ] in
  let new_log = { last_log with note = addendum } in
  Init.currentTreeref := Lumber.replace_last_log tree new_log;
  write_tree ()

let get_last_year () =
  let date : tm = getDate in
  let date_last_year = { date with tm_year = date.tm_year - 1 } in
  format_date_dmy date_last_year |> find_logs

let main (args : string array) =
  if Array.length args < 2 then raise (Failure "No text file specified")
  else
    let speclist =
      [
        ("-i", Arg.String Init.init, "Creates Entry Tree");
        ("-a", Arg.Unit amend, "Amend previous note");
        ("-p", Arg.Unit print_tree, "Prints all logs");
        ("-f", Arg.String find_logs, "Finds all logs containing keyword");
        ( "-m",
          Arg.Unit print_metrics,
          "Prints character count metrics from past months" );
        ("-nc", Arg.Unit node_count, "Prints the total number of logs");
        ( "-fc",
          Arg.String print_count,
          "Prints the number of notes containing keyword" );
        ("-r", Arg.Unit gen_rand, "Generate a random note template");
        ("-n", Arg.Unit make_note, "log");
        ("-gr", Arg.Unit gen_gratitude, "Gratitude Journaling prompt");
        ("-ly", Arg.Unit get_last_year, "Prints notes from today last year");
      ]
    in
    let usage_msg = "Currently supported options include:" in
    Arg.parse speclist print_endline usage_msg

let () = main Sys.argv
