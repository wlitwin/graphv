module type S = sig 
    type t

    module Buffer : Buffer.S 

    module Dyn : Sigs.DynArrayS

    module VertexBuffer : sig
        type t = { arr : Dyn.t; mutable size : int }
        val create : unit -> t
        val clear : t -> unit
        val iteri : t -> f:(int -> float -> unit) -> unit
        val iter : t -> f:(float -> unit) -> unit
        val num_verts : t -> int
        val capacity : t -> int
        val iterv : t -> f:(float -> float -> float -> float -> unit) -> unit
        val check_size : t -> int -> unit
        val set : t -> int -> float -> float -> float -> float -> unit
        val get : t -> int -> float * float * float * float
        val num_bytes : t -> int
        val num_floats : t -> int
        val unsafe_array : t -> Dyn.underlying
        module Sub :
          sig
            type parent = t
            type t = Dyn.Sub.sub
            val empty : t
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

    module Path : sig
        type t = {
            mutable first : int;
            mutable count : int;
            mutable closed : bool;
            mutable nbevel : int;
            mutable fill : VertexBuffer.Sub.t;
            mutable stroke : VertexBuffer.Sub.t;
            mutable winding : Winding.t;
            mutable convex : bool;
        }

        val create : unit -> t
        val reset : t -> unit
    end

    type gl
    val create : flags:CreateFlags.t -> gl -> t option

    val edge_antialias : t -> bool
    val create_texture : t -> type_:[`RGBA | `Alpha] -> w:int -> h:int -> flags:ImageFlags.t -> data:Buffer.UByte.t -> int option
    val delete_texture : t -> image:int -> bool
    val update_texture : t -> image:int -> x:int -> y:int -> w:int -> h:int -> data:Buffer.UByte.t -> bool
    val get_texture_size : t -> image:int -> int*int
    val viewport : t -> width:float -> height:float -> dpi:float -> unit
    val cancel : t -> unit
    val flush : t -> VertexBuffer.t -> unit
    val fill : 
        t 
        -> paint:Paint.t 
        -> composite_op:CompositeOperationState.t 
        -> scissor:Scissor.t 
        -> fringe:float 
        -> bounds:Bounds.t
        -> paths:Path.t DynArray.t
        -> verts:VertexBuffer.t
        -> unit

    val stroke : 
        t 
        -> paint:Paint.t
        -> composite_op:CompositeOperationState.t
        -> scissor:Scissor.t
        -> fringe:float
        -> stroke_width:float
        -> paths:Path.t DynArray.t
        -> unit

    val triangles : 
        t
        -> paint:Paint.t
        -> composite_op:CompositeOperationState.t
        -> scissor:Scissor.t
        -> fringe:float
        -> vertices:VertexBuffer.Sub.t
        -> unit
end 

