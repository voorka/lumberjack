open Types
open Unix

val addLog: tree -> lumber -> tree

val addLogs: lumber list -> tree

val getLog: tm -> tree -> lumber option

val getRangeLogs: tm -> tm -> tree -> lumber list