open Parser
open Lumber
open Unix
open Types

let currentTreeref = ref (Lumber.add_logs [])

let file_ref = ref ""

let init_tree lumberList = currentTreeref := Lumber.add_logs lumberList

let init txt =
  init_tree (Parser.txtToLumberList txt);
  file_ref := txt
