open Parser
open Pervasives
open Types
open Unix

(* Returns if old_date is [EQ|LT|GT] new_date *)
let compare (old_date : tm) (new_date : tm) =
  let old_time =
    mktime
      {
        old_date with
        tm_year = old_date.tm_year - 1900;
        tm_mon = old_date.tm_mon - 1;
      }
  in
  let new_time =
    mktime
      {
        new_date with
        tm_year = new_date.tm_year - 1900;
        tm_mon = new_date.tm_mon - 1;
      }
  in
  if old_time = new_time then EQ else if old_time > new_time then GT else LT

(*  Returns height of tree *)
let height t = match t with Leaf -> 0 | Node (_, _, _, h) -> h

(*  Returns height different of 2 trees *)
let height_difference l r = Pervasives.abs (height l - height r)

(*  Rotates a tree left or right *)
let rec rotate tree =
  match tree with
  | Node (z, z_l, Node (y, y_l, Node (x, x_l, x_r, x_h), y_h), z_h) ->
      Node (y, Node (z, z_l, y_l, z_h - 2), Node (x, x_l, x_r, x_h), y_h)
  | Node (z, Node (y, y_l, Node (x, x_l, x_r, x_h), y_h), z_r, z_h) ->
      rotate (Node (z, Node (x, Node (y, y_l, x_l, y_h), x_r, x_h), z_r, z_h))
  | Node (z, Node (y, Node (x, x_l, x_r, x_h), y_r, y_h), z_r, z_h) ->
      Node (y, Node (x, x_l, x_r, x_h), Node (z, y_r, z_r, z_h - 2), y_h)
  | Node (z, z_l, Node (y, Node (x, x_l, x_r, x_h), y_r, y_h), z_h) ->
      rotate
        (Node (z, z_l, Node (x, x_l, Node (y, x_r, y_r, y_h + 1), x_h - 1), z_h))
  | _ -> raise (Failure "Did not match rotation cases")

(*  Balances an input tree based on AVL rules *)
let rec balance tree =
  match tree with
  | Leaf -> tree
  | Node (t, l, r, h) -> if height_difference l r > 1 then rotate tree else tree

(*  Adds lumber to tree *)
let rec add_log tree lum =
  let d = lum.date in
  match tree with
  | Leaf -> Node (lum, Leaf, Leaf, 1)
  | Node ({ date = old_d; note = old_n; tags = old_tags }, l, r, h) -> (
      match compare old_d d with
      | EQ ->
          let offset_lumber = { d with tm_sec = d.tm_sec + 1 } in
          balance
            (Node
               ( { date = old_d; note = old_n; tags = old_tags },
                 l,
                 add_log r { lum with date = offset_lumber },
                 h + 1 ))
      | LT ->
          balance
            (Node
               ( { date = old_d; note = old_n; tags = old_tags },
                 l,
                 add_log r lum,
                 h + 1 ))
      | GT ->
          balance
            (Node
               ( { date = old_d; note = old_n; tags = old_tags },
                 add_log l lum,
                 r,
                 h + 1 )) )


(*  Adds list of lumber to tree *)
let add_logs llist : tree = List.fold_left add_log Leaf llist

(*  Returns node from tree with date *)
let rec get_log date tree =
  match tree with
  | Leaf -> None
  | Node ({ date = old_d; note = old_n; tags = old_tags }, l, r, h) -> (
      match compare old_d date with
      | EQ -> Some { date = old_d; note = old_n; tags = old_tags }
      | GT -> get_log date l
      | LT -> get_log date r )

(*  Returns list of nodes between dates in reverse order. See below*)
let rec get_logs dateBegin dateEnd tree (acc : lumber list) =
  match tree with
  | Leaf -> acc
  | Node ({ date = old_d; note = old_n; tags = old_tags }, l, r, h) -> (
      match compare old_d dateBegin with
      | EQ ->
          get_logs dateBegin dateEnd r
            ({ date = old_d; note = old_n; tags = old_tags } :: acc)
      | LT -> get_logs dateBegin dateEnd r acc
      | GT -> (
          match compare old_d dateEnd with
          | EQ -> get_logs dateBegin dateEnd l acc
          | LT ->
              get_logs dateBegin dateEnd r
                (get_logs dateBegin dateEnd l
                   ({ date = old_d; note = old_n; tags = old_tags } :: acc))
          | GT -> get_logs dateBegin dateEnd l acc ) )

(*  Returns list of nodes between dates in tree *)
let get_range_logs dateBegin dateEnd tree =
  List.rev (get_logs dateBegin dateEnd tree [])

let sort_lumber (lumber : lumber list) : lumber list =
  let int_compare x y =
    match compare x.date y.date with EQ -> 0 | GT -> 1 | LT -> -1
  in
  List.sort int_compare lumber

let find_all_notes keyword tree =
  let rec find_all (keyword : string) tree (acc : lumber list) =
    let find lumber : lumber list =
      try
        ignore
          (Str.search_forward
             (Str.regexp_string_case_fold keyword)
             lumber.note 0);
        lumber :: acc
      with Not_found -> acc
    in
    match tree with
    | Leaf -> acc
    | Node ({ date = old_d; note = old_n; tags = old_tags }, l, r, h) ->
        let l_acc =
          find_all keyword l
            (find { date = old_d; note = old_n; tags = old_tags })
        in
        find_all keyword r l_acc
  in
  find_all keyword tree [] |> sort_lumber

let rec get_earliest_date tree : tm =
  match tree with
  | Leaf -> getDate
  | Node (d, l, _, 1) -> d.date
  | Node (d, l, _, _) -> get_earliest_date l

let rec get_latest_date tree : tm =
  match tree with
  | Leaf -> getDate
  | Node (d, _, r, 1) -> d.date
  | Node (d, _, r, _) -> get_latest_date r

let get_month_list e l : tm list =
  let empty_date =
    {
      tm_mon = 0;
      tm_mday = 1;
      tm_year = 0;
      tm_hour = 0;
      tm_min = 0;
      tm_sec = 0;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in
  let rec get_months e l acc =
    if e.tm_year == l.tm_year then
      if e.tm_mon == l.tm_mon then
        { empty_date with tm_mon = e.tm_mon; tm_year = e.tm_year } :: acc
      else
        get_months e
          { l with tm_mon = l.tm_mon - 1 }
          ({ empty_date with tm_mon = l.tm_mon; tm_year = l.tm_year } :: acc)
    else if l.tm_mon == 1 then
      get_months e
        { l with tm_mon = 12; tm_year = l.tm_year - 1 }
        ({ empty_date with tm_mon = l.tm_mon; tm_year = l.tm_year } :: acc)
    else
      get_months e
        { l with tm_mon = l.tm_mon - 1 }
        ({ empty_date with tm_mon = l.tm_mon; tm_year = l.tm_year } :: acc)
  in
  get_months e l []

(* Returns length of input lumber *)
let get_character_count (x : int) (lumber : lumber) : int =
  x + String.length lumber.note

let rec get_character_count_list (months : tm list) tree : (tm * int) list =
  let rec get_character_counts months acc =
    match months with
    | h :: t ->
        let frequency =
          List.fold_left get_character_count 0
            (get_range_logs h { h with tm_mon = h.tm_mon + 1 } tree)
        in
        get_character_counts t ((h, frequency) :: acc)
    | _ -> acc
  in
  get_character_counts months []

let collect_metrics tree =
  let earliest = get_earliest_date tree in
  let latest = get_latest_date tree in
  let month_list = get_month_list earliest latest in
  List.rev (get_character_count_list month_list tree)
