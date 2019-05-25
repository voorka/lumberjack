open Parser
open Pervasives
open Types
open Unix

(* Returns if old_date is [EQ|LT|GT] new_date *)
let compare (old_date:tm) (new_date:tm) =
    let old_time = mktime old_date in  
    let new_time = mktime new_date in
    if old_time == new_time then EQ
    else if old_time > new_time then GT
    else LT

(*  Returns height of tree *)
let height t =
    match t with
    | Leaf -> 0
    | Node(_,_,_,h) -> h

(*  Returns height different of 2 trees *)
let height_difference l r =
    Pervasives.abs(height l - height r)

(*  Rotates a tree left or right *)
let rec rotate tree = 
    match tree with 
    | Node(z ,z_l, Node(y, y_l, Node(x,x_l, x_r, x_h), y_h), z_h) ->
        Node(y, Node(z, z_l, y_l, z_h-2), Node(x, x_l, x_r, x_h), y_h)
    | Node(z, Node(y,y_l, Node(x, x_l, x_r,x_h), y_h), z_r, z_h) ->
        rotate(Node(z, Node(x, Node(y,y_l,x_l, y_h),x_r,x_h), z_r, z_h))
    | Node(z, Node( y, Node(x, x_l ,x_r, x_h), y_r, y_h),z_r,z_h) -> 
        Node(y, Node(x, x_l,x_r,x_h), Node(z,y_r,z_r,z_h-2), y_h)
    | Node(z, z_l, Node(y, Node(x,x_l, x_r, x_h),y_r,y_h), z_h) ->
        rotate(Node(z, z_l, Node(x,x_l, Node(y, x_r, y_r, y_h+1),x_h-1), z_h))
    |_ -> raise (Failure "Did not match rotation cases")

(*  Balances an input tree based on AVL rules *)
let rec balance tree = 
    match tree with
    | Leaf -> tree
    | Node(t,l,r,h) -> if ((height_difference l r) > 1) then (rotate tree)
        else tree

(*  Adds lumber to tree *)
let rec add_log tree lum =
    let d = lum.date in
      match tree with
      | Leaf -> Node (lum, Leaf, Leaf, 1)
      | Node({date=old_d; note=old_n; tags=old_tags}, l, r, h) ->
        match (compare old_d d) with
        | EQ -> raise (Failure "Two notes have the same time. Something is wrong.")
        | LT -> balance(Node({date=old_d; note=old_n; tags=old_tags}, l, (add_log r lum), h+1))
        | GT -> balance(Node({date=old_d; note=old_n; tags=old_tags}, (add_log l lum), r, h+1))

(*  Adds list of lumber to tree *)
let add_logs llist :tree =
    List.fold_left (add_log) Leaf llist  

(*  Returns node from tree with date *)
let rec get_log date tree = 
      match tree with
      | Leaf -> None
      | Node({date=old_d; note=old_n; tags=old_tags}, l, r, h) ->
        match (compare old_d date) with
        | EQ -> Some {date=old_d; note=old_n; tags=old_tags}
        | GT -> get_log date l
        | LT -> get_log date r

(*  Returns list of nodes between dates in reverse order. See below*)
let rec get_logs dateBegin dateEnd tree (acc:lumber list)= 
      match tree with
      | Leaf -> acc
      | Node({date=old_d; note=old_n; tags=old_tags}, l, r, h) ->
        begin
            match (compare old_d dateBegin) with
            | EQ -> get_logs dateBegin dateEnd r ({date=old_d; note=old_n; tags=old_tags}::acc)
            | LT -> get_logs dateBegin dateEnd r acc
            | GT -> match (compare old_d dateEnd) with
                | EQ -> get_logs dateBegin dateEnd l ({date=old_d; note=old_n; tags=old_tags}::acc)
                | LT -> get_logs dateBegin dateEnd r (get_logs dateBegin dateEnd l ({date=old_d; note=old_n; tags=old_tags}::acc))
                | GT -> get_logs dateBegin dateEnd l acc
        end

(*  Returns list of nodes between dates in tree *)
let get_range_logs dateBegin dateEnd tree= 
    List.rev (get_logs dateBegin dateEnd tree [])

let find_all_notes keyword tree =
        let rec find_all (keyword:string) tree (acc:lumber list) =
        let find lumber :lumber list=
                try (Str.search_forward (Str.regexp_string keyword) lumber.note 0); (lumber::acc)
                with Not_found -> acc
                in
        match tree with
        | Leaf -> acc
        | Node({date=old_d; note=old_n; tags=old_tags}, l, r, h) ->
                let l_acc = find_all keyword l (find {date=old_d; note=old_n; tags=old_tags}) in
                find_all keyword r l_acc
        in
        find_all keyword tree []



