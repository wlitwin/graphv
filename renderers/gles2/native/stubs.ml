type float_buffer = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type blending_factor = int
type texture_target = int
type pixel_format = int
type pixel_type = int
type tex_filter = int
type wrap_mode = int
type tex_param_filter = int
type tex_param_wrap = int
type tex_param_filter_param = int
type tex_param_wrap_param = int
type pixel_store_param = int
type enable_cap = int
type depth_function = int
type stencil_op = int
type begin_mode = int
type cull_face_mode = int
type 'a uniform_location = int
type front_face_dir = int
type uniform_type = int
type buffer_target = int
type buffer_usage = int
type error_code = int
type texture = int
type data_type = int
type buffer = int
type buffer_id = int
type shader_type = int
type shader = int
type shader_param = int
type program_param = int

type enum = int

let shader_type : shader_param = 0x8B4F
let delete_status : shader_param = 0x8B80
let compile_status : shader_param = 0x8B81
let info_log_length : shader_param = 0x8B84
let shader_source_length : shader_param = 0x8B88

let link_status : program_param = 0x8B82
let prog_delete_status : program_param = delete_status
let validate_status : program_param = 0x8B83
let prog_info_log_length : program_param = info_log_length
let attached_shaders : program_param = 0x8B85
let active_attributes : program_param = 0x8B89
let active_attribute_max_length : program_param = 0x8B8A
let active_uniforms : program_param = 0x8B86
let active_uniforms_max_length : program_param = 0x8B87

let true_ = 1
let false_ = 0
let vertex_shader : shader_type = 0x8B31
let invalid_enum : error_code = 0x500
let fragment_shader : shader_type = 0x8B30
let src_alpha : blending_factor = 0x302
let dst_alpha : blending_factor = 0x304
let no_error : enum = 0
let zero : blending_factor = 0
let rgba : pixel_format = 0x1908
let luminance : pixel_format = 0x1909
let unsigned_byte : pixel_type = 0x1401
let nearest_mipmap_nearest : tex_param_filter_param = 0x2700
let linear_mipmap_linear : tex_param_filter_param = 0x2702
let nearest : tex_param_filter_param = 0x2600
let linear : tex_param_filter_param = 0x2601
let zero_ : stencil_op = 0
let one : blending_factor = 1
let src_color : blending_factor = 0x300
let dst_color : blending_factor = 0x306
let one_minus_src_color : blending_factor = 0x301
let one_minus_dst_color : blending_factor = 0x307
let one_minus_src_alpha : blending_factor = 0x303
let one_minus_dst_alpha : blending_factor = 0x305
let src_alpha_saturate : blending_factor = 0x308
let src_alpha : blending_factor = 0x302
let dst_alpha : blending_factor = 0x304
let texture_2d : texture_target = 0xDE1
let rgba : pixel_format = 0x1908
let luminance : pixel_format = 0x1909
let unsigned_byte : pixel_type = 0x1401
let nearest_mipmap_nearest : tex_param_filter_param = 0x2700
let linear_mipmap_linear : tex_param_filter_param = 0x2702
let nearest : tex_param_filter_param = 0x2600
let linear : tex_param_filter_param = 0x2601
let texture_min_filter : tex_param_filter = 0x2801
let texture_mag_filter : tex_param_filter = 0x2800
let clamp_to_edge : tex_param_wrap_param = 0x812F
let texture_wrap_s : tex_param_wrap = 0x2802
let texture_wrap_t : tex_param_wrap = 0x2803
let repeat : tex_param_wrap_param = 0x2901
let unpack_alignment : pixel_store_param = 0xCF5
let stencil_test : enable_cap = 0xB90
let equal : depth_function = 0x202
let keep : stencil_op = 0x1E00
let incr : stencil_op = 0x1E02
let triangle_strip : begin_mode = 0x5
let triangle_fan : begin_mode = 0x6
let always : depth_function = 0x207
let notequal : depth_function = 0x205
let cull_face_enum : enable_cap = 0xB44
let back : cull_face_mode = 0x405
let ccw : front_face_dir = 0x901
let blend : enable_cap = 0xBE2
let depth_test : enable_cap = 0xB71
let front : cull_face_mode = 0x404
let incr_wrap : stencil_op = 0x8507
let decr_wrap : stencil_op = 0x8508
let scissor_test : enable_cap = 0xC11
let texture0 : enum = 0x84C0
let float : data_type = 0x1406
let array_buffer : buffer_target = 0x8892
let triangles : begin_mode = 0x4
let stream_draw : buffer_usage = 0x88E0
let invalid_enum : error_code = 0x500

external cull_face : cull_face_mode -> unit = "gles2_cull_face"[@@noalloc]
external clear_color : float -> float -> float -> float -> unit = "gles2_clear_color"[@@noalloc]
external uniform4fv : [`vec4] uniform_location -> float_buffer -> unit = "gles2_uniform4fv"[@@noalloc]
external uniform2fv : [`vec2] uniform_location -> float_buffer -> unit = "gles2_uniform2fv"[@@noalloc]
external buffer_data : buffer_target -> float_buffer -> buffer_usage -> unit = "gles2_buffer_data"[@@noalloc]
external enable : enable_cap -> unit = "gles2_enable"[@@noalloc]
external disable : enable_cap -> unit = "gles2_disable"[@@noalloc]
external finish : unit -> unit = "gles2_finish"[@@noalloc]
external draw_arrays : begin_mode -> int -> int -> unit = "gles2_draw_arrays"[@@noalloc]
external color_mask : bool -> bool -> bool -> bool -> unit = "gles2_color_mask"[@@noalloc]
external front_face : front_face_dir -> unit = "gles2_front_face"[@@noalloc]
external active_texture : enum -> unit = "gles2_active_texture"[@@noalloc]
external stencil_mask : int -> unit = "gles2_stencil_mask"[@@noalloc]
external uniform1i : int uniform_location -> int -> unit = "gles2_uniform1i"[@@noalloc]
external stencil_func : depth_function -> int -> int -> unit = "gles2_stencil_func"[@@noalloc]
external stencil_op : stencil_op -> stencil_op -> stencil_op -> unit = "gles2_stencil_op"[@@noalloc]
external stencil_op_separate : cull_face_mode -> stencil_op -> stencil_op -> stencil_op -> unit = "gles2_stencil_op_separate"[@@noalloc]
external blend_func_separate : blending_factor -> blending_factor -> blending_factor -> blending_factor -> unit = "gles2_blend_func_separate"[@@noalloc]
external pixel_storei : pixel_store_param -> int -> unit = "gles2_pixel_storei"[@@noalloc]
external enable_vertex_attrib_array : int -> unit = "gles2_enable_vaa"[@@noalloc]
external disable_vertex_attrib_array : int -> unit = "gles2_disable_vaa"[@@noalloc]
external bind_buffer : buffer_target -> buffer_id -> unit = "gles2_bind_buffer"[@@noalloc]
external tex_parameteri_1 : texture_target -> tex_param_filter -> tex_param_filter_param -> unit = "gles2_tex_parameteri"[@@noalloc]
external tex_parameteri_2 : texture_target -> tex_param_wrap -> tex_param_wrap_param -> unit = "gles2_tex_parameteri"[@@noalloc]
external create_program : unit -> int = "gles2_create_program"[@@noalloc]

type program = int
external get_uniform_location : program -> string -> 'a uniform_location = "gles2_uniform_loc"[@@noalloc]

external use_program : program -> unit = "gles2_use_program"[@@noalloc]

external generate_mipmap : texture_target -> unit = "gles2_generate_mipmap"[@@noalloc]

external gen_textures : int array -> unit = "gles2_gen_textures"[@@noalloc]

external delete_textures : int array -> unit = "gles2_delete_textures"[@@noalloc]

external vertex_attrib_pointer : int -> int -> data_type -> bool -> int -> int -> unit = 
    "gles2_vertex_attrib_pointer_byte" "gles2_vertex_attrib_pointer"[@@noalloc]

external get_error : unit -> enum = "gles2_get_error"[@@noalloc]

external create_shader : shader_type -> shader = "gles2_create_shader"[@@noalloc]

external shader_source : shader -> string -> unit = "gles2_shader_source"[@@noalloc]

external compile_shader : shader -> unit = "gles2_compile_shader"[@@noalloc]

external attach_shader : program -> shader -> unit = "gles2_attach_shader"[@@noalloc]

external get_shaderiv : shader -> shader_param -> int = "gles2_get_shaderiv"[@@noalloc]

external link_program : program -> unit = "gles2_link_program"[@@noalloc]

external get_programiv : program -> program_param -> int = "gles2_get_programiv"[@@noalloc]

external bind_attrib_location : program -> int -> string -> unit = "gles2_bind_attrib_location"[@@noalloc]

external delete_program : program -> unit = "gles2_delete_program"[@@noalloc]

external delete_shader : shader -> unit = "gles2_delete_shader"[@@noalloc]

external get_shader_info_log : shader -> string = "gles2_get_shader_info_log"
external get_program_info_log : program -> string = "gles2_get_program_info_log"

external gen_buffers : int array -> unit = "gles2_gen_buffers"[@@noalloc]

external bind_texture : texture_target -> texture -> unit = "gles2_bind_texture"[@@noalloc]

(*

external texture_equal : texture option -> texture option -> bool


external check_error : string -> unit

external create_program : unit -> (program * locs) option
*)

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
}

module Buffer = struct
    module UByte = struct
        type t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

        let create size =
            let arr =
                Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size
            in
            Bigarray.Array1.fill arr 0;
            arr

        let get : t -> int -> int = Bigarray.Array1.unsafe_get
        let set : t -> int -> int -> unit = Bigarray.Array1.unsafe_set
        let length : t -> int = Bigarray.Array1.dim
        let sub : t -> int -> int -> t = Bigarray.Array1.sub
        let empty : t = create 0
    end

    module Float = struct
        type t = float_buffer

        let create size =
            let arr =
                Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout size
            in
            Bigarray.Array1.fill arr 0.;
            arr

        let get : t -> int -> float = Bigarray.Array1.unsafe_get
        let set : t -> int -> float -> unit = Bigarray.Array1.unsafe_set
        let length : t -> int  = Bigarray.Array1.dim
        let fill : t -> float -> unit  = Bigarray.Array1.fill
        let blit ~(src : t) ~(s_off : int) ~(dst : t) ~(d_off : int) ~(len : int) : unit = 
            let a = Bigarray.Array1.sub src s_off len in
            let b = Bigarray.Array1.sub dst d_off len in
            Bigarray.Array1.blit a b
    end
end

external tex_image2d : texture_target -> int -> pixel_format 
    -> int -> int -> int -> pixel_format -> pixel_type -> Buffer.UByte.t -> unit =
        "gles2_tex_image2d_byte" "gles2_tex_image2d"[@@noalloc]

external tex_sub_image2d : texture_target -> int -> int -> int -> int -> int -> pixel_format -> pixel_type -> Buffer.UByte.t -> unit = 
    "gles2_tex_subimage2d_byte" "gles2_tex_subimage2d"[@@noalloc]
