open Types
open Unix

val add_log: tree -> lumber -> tree

val add_logs: lumber list -> tree

val get_log: tm -> tree -> lumber option

val get_range_logs: tm -> tm -> tree -> lumber list

val find_all_notes: string -> tree -> lumber list

val collect_metrics: tree -> (tm * int) list