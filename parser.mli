open Types

val set_date_ref: string -> unit
val get_date_ref: string ref
val parse: string -> command

val txtToLumberList: string -> lumber list

val extractDate: string -> date