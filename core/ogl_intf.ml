module type S = sig
    module Buffer : Buffer.S

    module Dyn : sig
      type t
      type underlying = Buffer.Float.t
      val create : int -> t
      val clear : t -> unit
      val get : t -> int -> float
      val set : t -> int -> float -> unit
      val capacity : t -> int
      val length : t -> int
      val add_range : t -> int -> int
      val unsafe_array : t -> underlying
      module Sub :
        sig
          type sub
          val sub : t -> int -> int -> sub
          val offset : sub -> int
          val length : sub -> int
          val blit :
            src:sub -> dst:t -> src_start:int -> dst_start:int -> len:int -> unit
        end
    end

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

    type arg
    type t
    val create : arg -> t

    type blending_factor
    type texture_target
    type pixel_format
    type pixel_type
    type tex_filter
    type wrap_mode
    type tex_param_filter
    type tex_param_wrap
    type tex_param_filter_param
    type tex_param_wrap_param
    type pixel_store_param
    type enable_cap
    type depth_function
    type stencil_op
    type begin_mode
    type cull_face_mode
    type 'a uniform_location
    type front_face_dir
    type uniform_type
    type buffer_target
    type buffer_usage
    type error_code
    type texture
    type data_type
    type buffer
    type buffer_id

    type enum

    val zero : blending_factor
    val zero_ : stencil_op
    val one : blending_factor
    val src_color : blending_factor
    val dst_color : blending_factor
    val one_minus_src_color : blending_factor
    val one_minus_dst_color : blending_factor
    val one_minus_src_alpha : blending_factor
    val one_minus_dst_alpha : blending_factor
    val src_alpha_saturate : blending_factor
    val src_alpha : blending_factor
    val dst_alpha : blending_factor
    val texture_2d : texture_target
    val rgba : pixel_format
    val luminance : pixel_format
    val unsigned_byte : pixel_type
    val nearest_mipmap_nearest : tex_param_filter_param
    val linear_mipmap_linear : tex_param_filter_param
    val nearest : tex_param_filter_param
    val linear : tex_param_filter_param
    val texture_min_filter : tex_param_filter
    val texture_mag_filter : tex_param_filter
    val clamp_to_edge : tex_param_wrap_param
    val texture_wrap_s : tex_param_wrap
    val texture_wrap_t : tex_param_wrap
    val repeat : tex_param_wrap_param
    val unpack_alignment : pixel_store_param
    val stencil_test : enable_cap
    val equal : depth_function
    val keep : stencil_op
    val incr : stencil_op
    val triangle_strip : begin_mode
    val triangle_fan : begin_mode
    val always : depth_function
    val notequal : depth_function
    val cull_face_enum : enable_cap
    val back : cull_face_mode
    val ccw : front_face_dir
    val blend : enable_cap
    val depth_test : enable_cap
    val front : cull_face_mode
    val incr_wrap : stencil_op
    val decr_wrap : stencil_op
    val scissor_test : enable_cap
    val texture0 : enum
    val float : data_type
    val array_buffer : buffer_target
    val triangles : begin_mode
    val stream_draw : buffer_usage
    val invalid_enum : error_code

    val cull_face : t -> cull_face_mode -> unit
    val front_face : t -> front_face_dir -> unit

    val texture_equal : t -> texture option -> texture option -> bool

    val bind_texture : t -> texture_target -> texture option -> unit
    val active_texture : t -> enum -> unit

    val stencil_mask : t -> int -> unit
    val stencil_func : t -> depth_function -> int -> int -> unit
    val stencil_op : t -> stencil_op -> stencil_op -> stencil_op -> unit
    val stencil_op_separate : t -> cull_face_mode -> stencil_op -> stencil_op -> stencil_op -> unit

    val blend_func_separate : t -> blending_factor -> blending_factor -> blending_factor -> blending_factor -> unit

    val gen_textures : t -> int -> texture array
    val pixel_storei : t -> pixel_store_param -> int -> unit

    val enable_vertex_attrib_array : t -> int -> unit
    val disable_vertex_attrib_array : t -> int -> unit
    val vertex_attrib_pointer : t -> int -> int -> data_type -> bool -> int -> int -> unit

    type program
    val use_program : t -> program -> unit

    val uniform1i : t -> int uniform_location -> int -> unit
    val uniform2fv : t -> [`vec2] uniform_location -> Buffer.Float.t -> unit

    val tex_image2d : t -> texture_target -> int -> pixel_format -> int -> int -> int -> pixel_format -> pixel_type -> Buffer.UByte.t -> unit
    val tex_sub_image2d : t -> texture_target -> int -> int -> int -> int -> int -> pixel_format -> pixel_type -> Buffer.UByte.t -> unit
    val tex_parameteri_1 : t -> texture_target -> tex_param_filter -> tex_param_filter_param -> unit
    val tex_parameteri_2 : t -> texture_target -> tex_param_wrap -> tex_param_wrap_param -> unit
    val generate_mipmap : t -> texture_target -> unit
    val delete_textures : t -> texture array -> unit
    val uniform4fv : t -> [`vec4] uniform_location -> Buffer.Float.t -> unit

    val buffer_data : t -> buffer_target -> Buffer.Float.t -> buffer_usage -> unit

    val bind_buffer : t -> buffer_target -> buffer_id -> unit

    val draw_arrays : t -> begin_mode -> int -> int -> unit
    val color_mask : t -> bool -> bool -> bool -> bool -> unit

    val enable : t -> enable_cap -> unit
    val disable : t -> enable_cap -> unit
    val finish : t -> unit

    val get_uniform_location : t -> program -> string -> 'a uniform_location

    val check_error : t -> string -> unit

    type locs = {
        frag : [`vec4] uniform_location;
        tex : int uniform_location;
        view_size : [`vec2] uniform_location;
        vert_buf : buffer_id;
    }
    val create_program : t -> (program * locs) option
end 
(*
with type Buffer.UByte.t = B.ubyte_t 
and type Buffer.Float.t = B.float_t
*)
