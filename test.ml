(** Test our prime number algorithms *)

open Num
open Nat

let max_rand_int = 0x3FFFFFFF

let _ =
  Random.self_init ();
  let modlist = [ (15, 10);
                  (8, 4);
                  (1040, 981);
                  (677, 540);
                  (max_int, 5900) ] in
    (*Math.modexp_rtl b e m*)
    List.map
      (fun (x, y) ->
         let xn = nat_of_int x
         and yn = nat_of_int y in
           Math.mod_nat xn yn;
           Printf.printf "%d mod %d = %s (should be %d)\n%!"
             x y (string_of_nat xn) (x mod y))
      modlist;

    let melist = [ (5,9,8);
                   (5,12,13);
                   (3,9,477);
                   (7,3,91);
                   (3,6,7);
                   (2,18,19);
                   (21,3,87) ] in
      List.map
        (fun (b,e,m) ->
           let should_be = (int_of_float ((float_of_int b) ** (float_of_int e))) mod m
           and me_out = Math.modexp_rtl (Int b) (Int e) (Int m) in
             Printf.printf "%d^%d mod %d = %s (should be %d)\n%!"
               b e m (string_of_num me_out) should_be)
        melist;

  let b = Int 76001
  and e = Int 1900100
  and m = Int 1900101 in
  let out = Math.modexp_rtl b e m in
    Printf.printf "76001^1900100 mod 1900101 is %s\n%!" (string_of_num out);

    let out = Math.modexp_rtl (Int 4) (Int 13) (Int 497) in
      Printf.printf "4^13 mod 497 = %s (should be 445)\n%!" (string_of_num out);

    let primelist = [ Int 679; Int 977; Int 13; Int 1013; Int 677;
                      num_of_string "622288097498926496141095869268883999563096063592498055290461";
                      num_of_string "669483106578092405936560831017556154622901950048903016651289" ] in
      List.map 
        (fun x ->
           if Prime.miller_rabin 3 x then
             Printf.printf "MR: %s is prime!\n%!" (string_of_num x)
           else
             Printf.printf "MR: %s is not prime? Bad.\n%!" (string_of_num x);
           if Prime.fermat 6 x then
             Printf.printf "F: %s is prime!\n%!" (string_of_num x)
           else
             Printf.printf "F: %s is not prime? Bad.\n%!" (string_of_num x))
        primelist
;;

