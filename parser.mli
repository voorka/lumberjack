open Types
open Unix

val txtToLumberList: string -> lumber list

val extractDate: string -> tm * bool

val getRange: tm -> tm*tm

val getDate: tm