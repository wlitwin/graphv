open GlTypes

type vertex_array_object = int
let uniform_buffer_offset_alignment : enum = 0x8A34
let uniform_buffer : buffer_target = 0x8A11
let unpack_row_length : pixel_store_param = 0x0CF2
let unpack_skip_rows : pixel_store_param = 0x0CF3
let unpack_skip_pixels : pixel_store_param = 0x0CF4
let red : pixel_format = 0x1903
let r8 : pixel_format = 0x8229
let null_vao : vertex_array_object = 0
let dynamic_draw : buffer_usage = 0x88E8 

external buffer_data_null : buffer_target -> int -> buffer_usage -> unit = "gles_buffer_data_null"[@@noalloc]
external buffer_sub_data : buffer_target -> (*offset*) int -> (*size*) int -> float_buffer -> unit = "gles_buffer_sub_data"[@@noalloc]
external uniform4fv_offset : [`vec4] uniform_location -> float_buffer -> int -> int -> unit = "gles_uniform4fv_offset"[@@noalloc]
external get_integer : enum -> int = "gles_get_integer"[@@noalloc]
external uniform_block_binding : program -> int -> int -> unit = "gles_uniform_block_binding"[@@noalloc]
external bind_buffer_range : enum -> int -> int -> int -> int -> unit = "gles_bind_buffer_range"[@@noalloc]
external create_vertex_array_object : unit -> vertex_array_object = "gles_create_vertex_array_object"[@@noalloc]
external bind_vertex_array_object : vertex_array_object -> unit = "gles_bind_vertex_array_object"[@@noalloc]

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
}

