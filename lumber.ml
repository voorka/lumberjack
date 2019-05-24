open Parser
open Pervasives
open Types

(* Returns if old_date is [EQ|LT|GT] new_date *)
let compare old_date new_date =
    match old_date with
    | {month=old_month; day=old_day; year=old_year; hour=old_hour; minute=old_min; second=old_sec} -> 
        match new_date with
        | {month=month; day=day; year=year; hour=hour; minute=min; second=sec} ->
            let cmp_year = (Pervasives.compare old_year year) in 
            if( cmp_year == 0) then 
                let cmp_month = (Pervasives.compare old_month month) in 
                if (cmp_month == 0) then
                    let cmp_day = (Pervasives.compare old_day day) in 
                    if (cmp_day == 0) then
                        let cmp_hour = (Pervasives.compare old_hour hour) in 
                        if (cmp_hour == 0) then 
                            let cmp_min = (Pervasives.compare old_min min) in 
                            if (cmp_min == 0) then 
                                let cmp_sec = (Pervasives.compare old_sec sec) in 
                                if (cmp_sec == 0) then EQ
                                else if (cmp_sec < 0) then LT
                                else GT
                            else if (cmp_min < 0) then LT
                            else GT
                        else if (cmp_hour < 0) then LT
                        else GT
                    else if (cmp_day < 0) then LT
                    else GT
                else if (cmp_month < 0) then LT
                else GT
            else if (cmp_year < 0) then LT
            else GT 

let height t =
    match t with
    | Leaf -> 0
    | Node(_,_,_,h) -> h

let height_difference l r =
    Pervasives.abs(height l - height r)

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

let rec balance tree = 
    match tree with
    | Leaf -> tree
    | Node(t,l,r,h) -> if ((height_difference l r) > 1) then (rotate tree)
                       else tree

let rec addLog tree lum =
    let d = lum.date in
      match tree with
      | Leaf -> Node (lum, Leaf, Leaf, 1)
      | Node({date=old_d; note=old_n; tags=old_tags}, l, r, h) ->
        match (compare old_d d) with
        | EQ -> raise (Failure "Two notes have the same time. Something is wrong.")
        | LT -> balance(Node({date=old_d; note=old_n; tags=old_tags}, l, (addLog r lum), h+1))
        | GT -> balance(Node({date=old_d; note=old_n; tags=old_tags}, (addLog l lum), r, h+1))

let addLogs llist :tree =
    List.fold_left (addLog) Leaf llist  

let rec getLog date tree = 
      match tree with
      | Leaf -> None
      | Node({date=old_d; note=old_n; tags=old_tags}, l, r, h) ->
        match (compare old_d date) with
        | EQ -> Some {date=old_d; note=old_n; tags=old_tags}
        | LT -> getLog date r
        | GT -> getLog date l

