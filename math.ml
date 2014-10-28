(* Various important math functions *)

open Num
open Nat

let zero  = num_of_int 0
let one   = num_of_int 1
let two   = num_of_int 2
let three = num_of_int 3
let four  = num_of_int 4
let ten   = num_of_int 10

(* Calculate n1 % n2 and store results in n1 - solution from big_int.ml *)
let mod_nat n1 n2 =
  let size_n1 = (length_nat n1)
  and size_n2 = (length_nat n2) in
  let size_r = succ (max size_n1 size_n2) in
  let r = create_nat size_r in
    blit_nat r 0 n1 0 size_n1;
    set_to_zero_nat r size_n1 (size_r - size_n1);

    (* do the division of |n1| by |n2|
       - at the beginning, r contains |n1|
       - at the end, r contains
         * in the size_n2 least significant digits, the remainder
         * in the size_r-size_n2 most significant digits, the quotient
           note the conditions for application of div_nat are verified here
    *)
    div_nat r 0 size_r n2 0 size_n2;

    (* Extract the remainder (that's all we want) *)
    set_to_zero_nat n1 0 size_n1;
    blit_nat n1 0 r 0 size_n2;
    ()

(* Very fast right-to-left binary modular exponentiation.
   The modulo operator (above) probably still needs some speed
   work. *)
let modexp_rtl b e m =
  let btmp = nat_of_num b in
  let enat = nat_of_num e in
  let mnat = nat_of_num m in

  (* keep tmp and r the same size *)
  let tmp_len = (max (length_nat mnat) (length_nat btmp)) * 2 in
  let r_len = (max (length_nat btmp) (length_nat mnat)) * 2 
  and bnat_len = tmp_len in
  let tmp = make_nat tmp_len
  and zero = nat_of_int 0
  and r = make_nat r_len 
  and bnat = make_nat bnat_len in
    (* initial r value *)
    set_to_zero_nat r 0 r_len;
    set_digit_nat r 0 1;

    (* a copy of b -- need more room *)
    set_to_zero_nat bnat 0 bnat_len;
    blit_nat bnat 0 btmp 0 (length_nat btmp);

    while gt_nat enat 0 (length_nat enat) zero 0 1 do
      set_to_zero_nat tmp 0 tmp_len;
      if (1 land (nth_digit_nat enat 0)) = 1 then begin
        ignore(mult_nat tmp 0 tmp_len r 0 r_len bnat 0 bnat_len);
        mod_nat tmp mnat;
        set_to_zero_nat r 0 r_len;
        blit_nat r 0 tmp 0 tmp_len;
      end;
      shift_right_nat enat 0 (length_nat enat) tmp 0 1;
      set_to_zero_nat tmp 0 tmp_len;
      ignore(square_nat tmp 0 tmp_len bnat 0 bnat_len);
      mod_nat tmp mnat;
      set_to_zero_nat bnat 0 bnat_len;
      blit_nat bnat 0 tmp 0 tmp_len;
    done;
    num_of_nat r

(* Discover if an integer num is happy.
   See http://en.wikipedia.org/wiki/Happy_number 
   This code is not mine.  See http://rosettacode.org/wiki/Happy_numbers#OCaml
*)
let is_happy num =
  let step =
    let rec aux s n =
      if n =/ zero then s else
	let q = quo_num n ten
	and r = mod_num n ten
	in aux (s +/ (r */ r)) q
    in aux zero
  in
  let rec aux x y =
    if x =/ y then x else aux (step x) (step (step y))
  in 
    (aux num (step num)) =/ one


    
