include Stubs

type t = unit

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

include GlNativeOverride

let check_error () = Gl_utils.check_error

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
