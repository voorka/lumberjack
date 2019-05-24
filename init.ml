open Parser
open Lumber
open Unix
open Types

let currentTreeref = ref (Lumber.addLogs [] )

let initTree lumberList=
    currentTreeref := (Lumber.addLogs lumberList )

let init txt =
    initTree (Parser.txtToLumberList txt)
