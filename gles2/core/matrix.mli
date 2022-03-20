type t = {
    mutable m0 : float;
    mutable m1 : float;
    mutable m2 : float;
    mutable m3 : float;
    mutable m4 : float;
    mutable m5 : float;
}
val create : unit -> t
val copy : t -> t
val zero : t -> unit
val translate : t -> x:float -> y:float -> unit
val get_average_scale : t -> float
val multiply : dst:t -> src:t -> unit
val transform_point : t -> float -> float -> float*float
val premultiply : dst:t -> src:t -> unit
val scale : t -> xs:float -> ys:float -> unit
val inverse : dst:t -> src:t -> unit
val rotate : t -> angle:float -> unit
val identity : t -> unit
val skew_x : t -> angle:float -> unit
val skew_y : t -> angle:float -> unit
val to_3x4 : t -> float array
val is_flipped : t -> bool
val print : t -> unit
