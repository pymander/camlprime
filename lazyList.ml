(*
  A lazy list type
*)

open Lazy

type 'a t =
  | Node of 'a * 'a t lazy_t
  | Empty

(*
  make genfn ctrfn ctr init
  A lazy list needs a generator function, an incrementing function, a counter, and an incrementer.
*)
let make genfn ctrfn ctr init =
  let rec aux count last =
    let node = genfn count last in
      Node(node, lazy(aux (ctrfn count) node))
  in
    Node(init, lazy(aux ctr init))

let head ll =
  match ll with
    | Node(x, _) -> x
    | _ -> failwith("Empty lazy list on head")

let hd = head

let tail ll =
  match ll with
    | Node(_, lazy x) -> x
    | _ -> failwith("Empty lazy list on tail")

let tl = tail

let nth ll n =
  if n < 0 then invalid_arg "List.nth" else
    let rec nth_aux ll n =
      match ll with
        | Empty -> failwith "Empty lazy list on nth"
        | Node(hd, _) -> if n = 0 then hd else nth_aux (tail ll) (pred n)
    in 
      nth_aux ll n

(*
  map fn ll
  Actually returns another lazy list with functions operating on the first.
*)
let map fn ll =
  let rec map_aux l =
    match l with
      | Empty -> failwith "End of lazy list"
      | Node(hd, lazy tl) ->
          let node = fn hd in
            Node(node, lazy(map_aux tl))
  in 
    map_aux ll

(*
  Fold n elements
*)
let rec foldn_left fn n init ll =
  if n = 0 then init
  else match ll with
      | Empty -> init
      | Node(hd, lazy tl) ->
          foldn_left fn (pred n) (fn init hd) tl

let map2 fn l1 l2 =
  let rec map2_aux l1 l2 =
    match l1, l2 with
      | _, Empty 
      | Empty, _ -> failwith "End of lazy list"
      | Node(hd1,lazy tl1), Node(hd2,lazy tl2) ->
          let node = fn hd1 hd2 in
            Node(node, lazy(map2_aux tl1 tl2))
  in 
    map2_aux l1 l2

(* List scanning *)

(*
  memn n itm ll
  Scan next n nodes to see if itm is in ll
*)
let rec memn n itm ll =
  if n = 0 then false
  else 
    match ll with
      | Empty -> false
      | Node(hd, lazy tl) ->
          if hd = itm then true
          else memn (pred n) itm tl

(* End of lazyList.ml *)
