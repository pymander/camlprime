(**
   Random number generator for the Num module
*)
open Num

let a = Int 16807
let m = num_of_string "2147483647"

type t = {
  mutable list: Num.num LazyList.t;
  seed: Num.num
}

let rand_num rng bound =
  let rn = LazyList.head rng.list in
    rng.list <- LazyList.tail rng.list;
    mod_num rn bound

let rand_int rng bound =
  let rn = LazyList.head rng.list in
    rng.list <- LazyList.tail rng.list;
    int_of_num (mod_num rn (num_of_int bound))

let next_rand c n =
  mod_num ((a */ n) +/ c) m

(*
  Make a random bigint list with seed.
*)
let make seed =
  { seed = seed;
    list = LazyList.make next_rand (add_num (num_of_int 1)) (num_of_int 10) seed }
