open Js_of_ocaml
open GlTypes

type vertex_array_object = int Js.opt
let uniform_buffer : buffer_target = Js.Unsafe.pure_js_expr "0x8A11"
let dynamic_draw : buffer_usage = Js.Unsafe.pure_js_expr "0x88E8"
let uniform_buffer_offset_alignment : int WebGL.parameter = Js.Unsafe.pure_js_expr "0x8A34"
let unpack_row_length : pixel_store_param = Js.Unsafe.pure_js_expr "0x0CF2"
let unpack_skip_rows : pixel_store_param = Js.Unsafe.pure_js_expr "0x0CF3"
let unpack_skip_pixels : pixel_store_param = Js.Unsafe.pure_js_expr "0x0CF4"
let red : pixel_format = Js.Unsafe.pure_js_expr "0x1903"
let r8 : pixel_format = Js.Unsafe.pure_js_expr "0x8229"
let null_vao : vertex_array_object = Js.null

let create_vertex_array_object (c : t) : vertex_array_object =
  let open Js.Unsafe in
  (coerce c)##createVertexArray
;;

let bind_vertex_array_object (c : t) (vao : vertex_array_object) : unit =
  let open Js.Unsafe in
  (coerce c)##bindVertexArray(vao)
;;

let uniform_block_binding (c : t) (p : program) (a : 'a WebGL.uniformLocation Js.t) (b : int) : unit =
    let open Js.Unsafe in
    (coerce c)##uniformBlockBinding p a b
;;

let get_integer (c : t) p =
    c##getParameter p
;;

let bind_buffer_range (c : t) (target : buffer_target) (index : int) (buffer : buffer_id) (offset : int) (size : int) : unit =
    let open Js.Unsafe in
    (coerce c)##bindBufferRange target index buffer offset size
;;

let buffer_data (c : t) (t : buffer_target) (buffer : buffer) (size : int) (b : buffer_usage) : unit =
    let open Js.Unsafe in
    (coerce c)##bufferData t buffer b 0 (size/4)

let buffer_data_null (c : t) (t : buffer_target) (size : int) (b : buffer_usage) : unit =
    let open Js.Unsafe in
    (coerce c)##bufferData t Js.null b 0 (size/4)

let buffer_sub_data (c : t) (t : buffer_target) (offset : int) (size : int) (data : buffer) : unit =
    let open Js.Unsafe in
    (coerce c)##bufferSubData t offset data offset (size/4)
;;

let uniform4fv_offset (c : t) loc (values : Buffer.Float.t) (offset : int) (size : int) =
    c##uniform4fv_typed loc (values##subarray (offset) (offset + size))
;;

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
    frag_buf : buffer_id;
}
