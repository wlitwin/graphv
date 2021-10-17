module type UByteS = sig
    type t

    val set : t -> int -> int -> unit
    val sub : t -> int -> int -> t
    val length : t -> int
    val get : t -> int -> int
    val create : int -> t
    val empty : t
end


module type S = sig
    module UByte : UByteS

    module Float : sig
        type t
        
        val set : t -> int -> float -> unit
        val length : t -> int
        val get : t -> int -> float
        val create : int -> t
        val blit : src:t -> s_off:int -> dst:t -> d_off:int -> len:int -> unit
        val fill : t -> float -> unit
    end
end


