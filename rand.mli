val a : Num.num
val m : Num.num
type t = { mutable list : Num.num LazyList.t; seed : Num.num; }
val rand_num : t -> Num.num -> Num.num
val rand_int : t -> int -> int
val next_rand : Num.num -> Num.num -> Num.num
val make : Num.num -> t
