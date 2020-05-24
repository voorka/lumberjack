open Types
open Unix

val write_lines : string -> string list -> unit

val txtToLumberList : string -> lumber list

val extractDate : string -> tm * bool

val getRange : tm -> tm * tm

val getDate : tm

val format_date : tm -> string
