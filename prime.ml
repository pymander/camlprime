(**
   Lazy list for generating primes.  Very simple.
*)

open Lazy
open Num

let max_rand_int = 0x3FFFFFFF

(** Miller-Rabin primality test structure *)
module MRlist = struct
  type 'a t =
    | Rand of Rand.t
    | List of 'a list

  let ps_list =
    [ ((num_of_int 1_373_653), [ 2; 3 ]);
      ((num_of_int 9_080_191), [ 31; 73 ]);
      ((num_of_string "4759123141"), [ 2; 7; 61 ]);
      ((num_of_string "2152302898747"), [ 2; 3; 5; 7; 11 ]);
      ((num_of_string "3474749660383"), [ 2; 3; 5; 7; 11; 13 ]);
      ((num_of_string "341550071728321"), [ 2; 3; 5; 7; 11; 13; 17 ]) ]

  (** The Miller-Rabin test can use a pretty short list if it would like. *)
  let make n =
    let rec aux lst =
      match lst with
          (a, b) :: tl ->
            if n </ a then List(List.map num_of_int b)
            else aux tl
        | [] -> Rand(Rand.make (num_of_int (Random.int max_rand_int)))
    in
      aux ps_list

  let get lst n =
    match lst with
      | List(l) -> List.hd l
      | Rand(r) -> (Rand.rand_num r n) +/ (Int 2)

  let hd lst =
    match lst with
      | List(l) -> List.hd l
      | Rand(r) -> Rand.rand_num r

  let tl lst =
    match lst with
        List(l) -> List(List.tl l)
      | Rand(r) -> lst

  let nth lst idx =
    match lst with
        List(l) -> List.nth l idx
      | Rand(r) -> Rand.rand_num r

  let length lst =
    match lst with
        List(l) -> List.length l
      | Rand(r) -> (-1)

end

type maybe =
  | Prime
  | Composite
  | Maybe

(* Might look silly, but it's so much easier. *)
let zero  = num_of_int 0
let one   = num_of_int 1
let two   = num_of_int 2
let three = num_of_int 3
let four  = num_of_int 4

(* Babylonian/Newton method for integer square root approximation. *)
let newton_sqrt s =
  let d = String.length (string_of_num s) in
  let n = d / 3 in
  let x0 = (num_of_int d) */ ((num_of_int 10) **/ (num_of_int n)) in
  let rec aux xn =
    let x = (xn +/ (s // xn)) // two in
      if abs_num(x -/ xn) </ one then floor_num x
      else aux x
  in
    aux x0

let isqrt x =
  num_of_big_int (Big_int.sqrt_big_int (big_int_of_num x))

let mr_factor n =
  let n1 = n -/ one in
  let rec aux lst s =
    let two_s = two **/ s in
    let d = n1 // two_s in
      if (mod_num n1 two_s) =/ zero then
        if (mod_num d two) =/ one then aux (s, d) (s +/ one)
        else aux lst (s +/ one)
      else lst
  in
    aux (zero, zero) one

  (*
Input: n > 3, an odd integer to be tested for primality; 
Input: k, a parameter that determines the accuracy of the test
Output: composite if n is composite, otherwise probably prime
write n - 1 as 2^s * d with d odd by factoring powers of 2 from n - 1
LOOP: repeat k times:
   pick a randomly in the range [2, n - 2]
   x <- a^d mod n
   if x = 1 or x = n - 1 then do next LOOP
   for r = 1 .. s - 1
      x <- x^2 mod n
      if x = 1 then return composite
      if x = n - 1 then do next LOOP
   return composite
return probably prime
  *)
  let miller_rabin k n =
    match n with
        Int(1) -> false
      | Int(2) -> true
      | Int(3) -> true
      | _ ->
          let (s, d) = mr_factor n in
          let rec aux2 r x n =
            if r =/ zero then Composite
            else
              let x2 = (Math.modexp_rtl x two n) in
                if x2 =/ one then (Thread.yield (); Composite)
                else if x2 =/ n -/ one then (Thread.yield (); Maybe)
                else aux2 (r -/ one) x2 n
          in
          let rec aux rng k n =
            if k = 0 then true
            else
              let a = MRlist.get rng n in
              let x = Math.modexp_rtl a d n in
                if x =/ one || x =/ (n -/ one) then aux (MRlist.tl rng) (k - 1) n
                else match aux2 (s -/ one) x n with
                  | Maybe -> aux (MRlist.tl rng) (k - 1) n
                  | _ -> false
          in
            if mod_num n two =/ zero then false
            else
              let test_list = MRlist.make n in
                if MRlist.length test_list > 0 then aux test_list (MRlist.length test_list) n
                else aux test_list k n

  (* Fermat primality test
Inputs: n: a value to test for primality; k: a parameter that determines the number of times to test for primality
Output: composite if n is composite, otherwise probably prime
repeat k times:
   pick a randomly in the range (1, n − 1]
   if a^(n-1) mod n != 1, then return composite
return probably prime
  *)
  let fermat k n =
    let rec aux rng ctr k =
      if k = 0 then ctr
      else
        let a = ((Rand.rand_num rng) (n -/ two)) +/ one in
        let out = Math.modexp_rtl a (n -/ one) n in
          if out <>/ one then aux rng ctr (pred k)
          else aux rng (succ ctr) (pred k)
    in 
      if (aux (Rand.make (num_of_int (Random.int max_rand_int))) 0 k) > 0 then
        true
      else false

  let naive n =
    match n with
        Int(1) -> false
      | Int(2) -> true
      | Int(3) -> true
      | _ ->
          if mod_num n two =/ zero then false
          else
            let last = isqrt n in
              if mod_num n last =/ zero then false
              else
                let rec aux ctr i =
                  if ctr mod 50 = 0 then begin
                    Thread.yield ();
                  end;
                  if i >/ last then true
                  else if mod_num n i =/ zero then false
                  else aux (succ ctr) (i +/ two)
                in
                  aux 0 three
      
  let next_prime fn n =
    let rec aux x =
      if fn x then
        x
      else
        aux (x +/ two)
    in
      if mod_num n two =/ zero then aux (succ_num n)
      else aux n

  let make fn x =
    let np = next_prime fn in
    let f x y =
      np (succ_num y)
    in
    LazyList.make f (fun x -> x) 0 (np x)

  let head pl =
    LazyList.hd pl

  let tail pl =
    LazyList.tl pl

  let nth l n =
    LazyList.nth l n
