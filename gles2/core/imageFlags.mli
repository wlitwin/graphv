type t
val no_flags : t
val generate_mipmaps : t
val repeat_x : t
val repeat_y : t
val flip_y : t
val premultiplied : t
val nearest : t
val (lor) : t -> t -> t
val remove : t -> flag:t -> t
val has : t -> flag:t -> bool
