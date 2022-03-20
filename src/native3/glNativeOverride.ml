open GlTypes
open Gl_utils
open Stubs

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
    frag_buf : buffer_id;
}

let create_program (_t : t) =
    let shader = create_shaders() in
    match shader with
    | None -> None
    | Some shader ->
        let bufs = [|0; 0|] in
        Stubs.gen_buffers bufs;
        Some (shader.prog, {
            frag = Hashtbl.find shader.locs "frag";
            tex = Hashtbl.find shader.locs "tex";
            view_size = Hashtbl.find shader.locs "viewSize";
            vert_buf = bufs.(0);
            frag_buf = bufs.(1);
        })
    ;;

let buffer_data (_t : t) target buffer how =
    buffer_data target buffer how
;;

let uniform4fv_offset (_t : t) loc buffer off size =
    uniform4fv_offset loc buffer off size
;;

let buffer_data_null (_t : t) target size how =
    buffer_data_null target size how
;;

let buffer_sub_data (_t : t) target offset size data =
    buffer_sub_data target offset size data
;;

let get_integer (_t : t) b = get_integer b
let uniform_block_binding (_t : t) b c = uniform_block_binding b c
let bind_buffer_range (_t : t) b c d e f = bind_buffer_range b c d e f
let create_vertex_array_object (_t : t) = create_vertex_array_object()
let bind_vertex_array_object (_t : t) vao = bind_vertex_array_object vao
