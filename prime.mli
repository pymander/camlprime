module MRlist :
  sig
    type 'a t = Rand of Rand.t | List of 'a list
    val ps_list : (Num.num * int list) list
    val make : Num.num -> Num.num t
    val get : Num.num t -> Num.num -> Num.num
    val hd : (Num.num -> Num.num) t -> Num.num -> Num.num
    val tl : 'a t -> 'a t
    val nth : (Num.num -> Num.num) t -> int -> Num.num -> Num.num
    val length : 'a t -> int
  end
type maybe = Prime | Composite | Maybe
val zero : Num.num
val one : Num.num
val two : Num.num
val three : Num.num
val four : Num.num
val newton_sqrt : Num.num -> Num.num
val isqrt : Num.num -> Num.num
val mr_factor : Num.num -> Num.num * Num.num
val miller_rabin : int -> Num.num -> bool
val fermat : int -> Num.num -> bool
val naive : Num.num -> bool
val next_prime : (Num.num -> bool) -> Num.num -> Num.num
val make : (Num.num -> bool) -> Num.num -> Num.num LazyList.t
val head : 'a LazyList.t -> 'a
val tail : 'a LazyList.t -> 'a LazyList.t
val nth : 'a LazyList.t -> int -> 'a
