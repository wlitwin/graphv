include Stubs

type arg = unit
type t = unit
let create (t : t) : t = t

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
    frag_buf : buffer_id;
}

let null_program = 0

let texture_equal (_t : t) (a : texture option) (b : texture option) : bool =
    match a, b with
    | None, None -> true
    | Some a, Some b -> Int.equal a b
    | _ -> false
;;

let max (a : int) (b : int) : int =
    if a < b then b else a

let uniform2fv (_t : t) loc buffer =
    uniform2fv loc buffer
;;

let uniform4fv (_t : t) loc buffer =
    uniform4fv loc buffer
;;

let create_program (_t : t) =
    let shader = Utils.create_shaders() in
    match shader with
    | None -> None
    | Some shader ->
        let bufs = [|0; 0;|] in
        gen_buffers bufs;
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

let check_error () = Utils.check_error

let enable (_t : t) a = enable a
let disable (_t : t) a = disable a
let draw_arrays (_t : t) a b c = draw_arrays a b c
let color_mask (_t : t) a b c d = color_mask a b c d
let active_texture (_t : t) a = active_texture a
let front_face (_t : t) a = front_face a
let stencil_mask (_t : t) a = stencil_mask a
let finish (_t : t) = finish ()
let cull_face (_t : t) m = cull_face m
let uniform1i (_t : t) l i = uniform1i l i
let stencil_op (_t : t) a b c = stencil_op a b c
let stencil_func (_t : t) d a b = stencil_func d a b
let stencil_op_separate (_t : t) a b c d = stencil_op_separate a b c d
let blend_func_separate (_t : t) a b c d = blend_func_separate a b c d
let pixel_storei (_t : t) a b = pixel_storei a b
let enable_vertex_attrib_array (_t : t) a = enable_vertex_attrib_array a
let disable_vertex_attrib_array (_t : t) a = disable_vertex_attrib_array a
let bind_buffer (_t : t) a b = bind_buffer a b
let use_program (_t : t) a = use_program a
let tex_parameteri_1 (_t : t) a b c = tex_parameteri_1 a b c
let tex_parameteri_2 (_t : t) a b c = tex_parameteri_2 a b c
let generate_mipmap (_t : t) a = generate_mipmap a
let get_uniform_location (_t : t) a b = get_uniform_location a b
let gen_textures a = gen_textures a
let get_integer (_t : t) b = get_integer b
let uniform_block_binding (_t : t) b c = uniform_block_binding b c
let bind_buffer_range (_t : t) b c d e f = bind_buffer_range b c d e f

let bind_texture (_t : t) (target : texture_target) value =
    let value =
        match value with
        | None -> 0
        | Some v -> v
    in
    bind_texture target value
;;


let gen_textures (_t : t) count =
    let buf = Array.make count 0 in
    gen_textures buf;
    buf
;;

let delete_textures (_t : t) arr =
    delete_textures arr
;;

let vertex_attrib_pointer (_t : t) index size type_ normalized stride offset =
    vertex_attrib_pointer index size type_ normalized stride offset
;;

let tex_image2d (_t : t) target level internal_format width height border format type_ data =
    tex_image2d target level internal_format width height border format type_ data

let tex_sub_image2d (_t : t) target level xoff yoff width height format typ_ data =
    tex_sub_image2d target level xoff yoff width height format typ_ data
;;
