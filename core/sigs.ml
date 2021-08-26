module type DynArrayS = sig
    type t
    type underlying
    val create : int -> t
    val clear : t -> unit
    val get : t -> int -> float
    val set : t -> int -> float -> unit
    val capacity : t -> int
    val length : t -> int
    val add_range : t -> int -> int
    val unsafe_array : t -> underlying

    module Sub : sig
        type sub

        val sub : t -> int -> int -> sub
        val offset : sub -> int
        val length : sub -> int
        val blit : src:sub -> dst:t -> src_start:int -> dst_start:int -> len:int -> unit
    end
end

module type VertexBufferS = sig
    type buffer
    type underlying
    type t = {
      arr : buffer;
      mutable size : int;
    }
    val create : unit -> t
    val clear : t -> unit
    val iteri : t -> f:(int -> float -> 'a) -> unit
    val iter : t -> f:(float -> 'a) -> unit
    val num_verts : t -> int
    val capacity : t -> int
    val iterv : t -> f:(float -> float -> float -> float -> 'a) -> unit
    val check_size : t -> int -> unit
    val set : t -> int -> float -> float -> float -> float -> unit
    val get : t -> int -> float * float * float * float
    val num_bytes : t -> int
    val num_floats : t -> int
    val unsafe_array : t -> underlying
    module Sub :
      sig
        type parent = t
        type t
        val sub : parent -> int -> int -> t
        val vertex_offset : t -> int
        val length : t -> int
        val blit :
          src:t ->
          dst:parent -> src_start:int -> dst_start:int -> len:int -> unit
        val num_verts : t -> int
        val create : unit -> t
      end
end
