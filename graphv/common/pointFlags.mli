type t = float
val no_flags : t
val corner : t
val left : t
val bevel : t
val inner_bevel : t
val has : t -> flag:t -> bool
val add : t -> flag:t -> t
val remove : t -> flag:t -> t
