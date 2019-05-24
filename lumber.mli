open Types

val addLog: tree -> lumber -> tree

val addLogs: lumber list -> tree

val getLog: date -> tree -> lumber option

val getRangeLogs: date -> date -> tree -> lumber list