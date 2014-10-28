type 'a t = Node of 'a * 'a t lazy_t | Empty
val make : ('a -> 'b -> 'b) -> ('a -> 'a) -> 'a -> 'b -> 'b t
val head : 'a t -> 'a
val hd : 'a t -> 'a
val tail : 'a t -> 'a t
val tl : 'a t -> 'a t
val nth : 'a t -> int -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val foldn_left : ('a -> 'b -> 'a) -> int -> 'a -> 'b t -> 'a
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val memn : int -> 'a -> 'a t -> bool
