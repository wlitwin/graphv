open Js_of_ocaml

type t = WebGL.renderingContext Js.t
type arg = WebGL.renderingContext Js.t
let create (t : t) : t = t

type enum = int
type blending_factor = WebGL.blendingFactor
type texture_target = WebGL.texTarget
type pixel_format = WebGL.pixelFormat
type pixel_type = WebGL.pixelType
type tex_filter = WebGL.texFilter
type tex_param_filter = WebGL.texFilter WebGL.texParam
type tex_param_wrap = WebGL.wrapMode WebGL.texParam
type tex_param_filter_param = WebGL.texFilter
type tex_param_wrap_param = WebGL.wrapMode
type wrap_mode = WebGL.wrapMode
type pixel_store_param = int WebGL.pixelStoreParam
type enable_cap = WebGL.enableCap
type depth_function = WebGL.depthFunction
type stencil_op = WebGL.stencilOp
type begin_mode = WebGL.beginMode
type cull_face_mode = WebGL.cullFaceMode
type front_face_dir = WebGL.frontFaceDir
type uniform_type = WebGL.uniformType
type buffer_target = WebGL.bufferTarget
type buffer_usage = WebGL.bufferUsage
type error_code = WebGL.errorCode
type texture = WebGL.texture Js.t
type data_type = WebGL.dataType
type buffer = Buffer.Float.t
type 'a uniform_location = 'a WebGL.uniformLocation Js.t
type buffer_id = WebGL.buffer Js.t

let zero : blending_factor = Js.Unsafe.pure_js_expr "0"
let zero_ : stencil_op = Js.Unsafe.pure_js_expr "0"
let one : blending_factor = Js.Unsafe.pure_js_expr "1"
let src_color : blending_factor = Js.Unsafe.pure_js_expr "0x300"
let dst_color : blending_factor = Js.Unsafe.pure_js_expr "0x306"
let one_minus_src_color : blending_factor = Js.Unsafe.pure_js_expr "0x301"
let one_minus_dst_color : blending_factor = Js.Unsafe.pure_js_expr "0x307"
let one_minus_src_alpha : blending_factor = Js.Unsafe.pure_js_expr "0x303"
let one_minus_dst_alpha : blending_factor = Js.Unsafe.pure_js_expr "0x305"
let src_alpha_saturate : blending_factor = Js.Unsafe.pure_js_expr "0x308"
let src_alpha : blending_factor = Js.Unsafe.pure_js_expr "0x302"
let dst_alpha : blending_factor = Js.Unsafe.pure_js_expr "0x304"
let texture_2d : texture_target = Js.Unsafe.pure_js_expr "0xDE1"
let rgba : pixel_format = Js.Unsafe.pure_js_expr "0x1908"
let luminance : pixel_format = Js.Unsafe.pure_js_expr "0x1909"
let unsigned_byte : pixel_type = Js.Unsafe.pure_js_expr "0x1401"
let nearest_mipmap_nearest : tex_param_filter_param = Js.Unsafe.pure_js_expr "0x2700"
let linear_mipmap_linear : tex_param_filter_param = Js.Unsafe.pure_js_expr "0x2702"
let nearest : tex_param_filter_param = Js.Unsafe.pure_js_expr "0x2600"
let linear : tex_param_filter_param = Js.Unsafe.pure_js_expr "0x2601"
let texture_min_filter : tex_param_filter = Js.Unsafe.pure_js_expr "0x2801"
let texture_mag_filter : tex_param_filter = Js.Unsafe.pure_js_expr "0x2800"
let clamp_to_edge : tex_param_wrap_param = Js.Unsafe.pure_js_expr "0x812F"
let texture_wrap_s : tex_param_wrap = Js.Unsafe.pure_js_expr "0x2802"
let texture_wrap_t : tex_param_wrap = Js.Unsafe.pure_js_expr "0x2803"
let repeat : tex_param_wrap_param = Js.Unsafe.pure_js_expr "0x2901"
let unpack_alignment : pixel_store_param = Js.Unsafe.pure_js_expr "0xCF5"
let stencil_test : enable_cap = Js.Unsafe.pure_js_expr "0xB90"
let equal : depth_function = Js.Unsafe.pure_js_expr "0x202"
let keep : stencil_op = Js.Unsafe.pure_js_expr "0x1E00"
let incr : stencil_op = Js.Unsafe.pure_js_expr "0x1E02"
let triangle_strip : begin_mode = Js.Unsafe.pure_js_expr "0x5"
let triangle_fan : begin_mode = Js.Unsafe.pure_js_expr "0x6"
let always : depth_function = Js.Unsafe.pure_js_expr "0x207"
let notequal : depth_function = Js.Unsafe.pure_js_expr "0x205"
let cull_face_enum : enable_cap = Js.Unsafe.pure_js_expr "0xB44"
let back : cull_face_mode = Js.Unsafe.pure_js_expr "0x405"
let ccw : front_face_dir = Js.Unsafe.pure_js_expr "0x901"
let blend : enable_cap = Js.Unsafe.pure_js_expr "0xBE2"
let depth_test : enable_cap = Js.Unsafe.pure_js_expr "0xB71"
let front : cull_face_mode = Js.Unsafe.pure_js_expr "0x404"
let incr_wrap : stencil_op = Js.Unsafe.pure_js_expr "0x8507"
let decr_wrap : stencil_op = Js.Unsafe.pure_js_expr "0x8508"
let scissor_test : enable_cap = Js.Unsafe.pure_js_expr "0xC11"
let texture0 : enum = Js.Unsafe.pure_js_expr "0x84C0"
let float : data_type = Js.Unsafe.pure_js_expr "0x1406"
let array_buffer : buffer_target = Js.Unsafe.pure_js_expr "0x8892"
let triangles : begin_mode = Js.Unsafe.pure_js_expr "0x4"
let stream_draw : buffer_usage = Js.Unsafe.pure_js_expr "0x88E0"
let invalid_enum : error_code = Js.Unsafe.pure_js_expr "0x500"

let blending_factor_equal : blending_factor -> blending_factor -> bool =
    let open Js.Unsafe in
    Js.Unsafe.pure_js_expr "function(a,b){return a==b}"
;;

let texture_equal (_c : t) (a : texture option) (b : texture option) : bool =
    match a, b with
    | None, None -> true
    | Some a, Some b -> a == b
    | _ -> false
;;

let cull_face (c : t) m = c##cullFace m
let front_face (c : t) m = c##frontFace m
let bind_texture (c : t) t v =
    match v with
    | Some v -> c##bindTexture t v
    | None -> ()
;;
let active_texture (c : t) t = c##activeTexture t

let enable (c : t) v = c##enable v
let disable (c : t) v = c##disable v

let stencil_mask (c : t) m = c##stencilMask m
let stencil_func (c : t) m a b = c##stencilFunc m a b
let stencil_op (c : t) a b d = c##stencilOp a b d
let stencil_op_separate (c : t) a b e d = c##stencilOpSeparate a b e d

let blend_func_separate (c : t) a b e d = c##blendFuncSeparate a b e d
let pixel_storei (c : t) t v =
    c##pixelStorei t v

let enable_vertex_attrib_array (c : t) v = c##enableVertexAttribArray v
let disable_vertex_attrib_array (c : t) v = c##disableVertexAttribArray v
let vertex_attrib_pointer (c : t) a b g d e f =
    c##vertexAttribPointer a b g (Js.bool d) e f

let tex_parameteri_1 (c : t) t a b = c##texParameteri t a b
let tex_parameteri_2 (c : t) t a b = c##texParameteri t a b

let buffer_data (c : t) t (buffer : buffer) size b =
    c##bufferData t (buffer##subarray 0 size) b

let tex_image2d (c : t) q w e r t y u i o =
    c##texImage2D_fromView q w e r t y u i o

let tex_sub_image2d (c : t) q w e r t y u i o =
    c##texSubImage2D_fromView q w e r t y u i o

let debug = false

let check_error (c : t) _str =
    if debug then (
        let error = c##getError in
        if error <> c##._NO_ERROR_ then (
            Printf.printf "WEBGL ERROR\n%!";
        )
    )
;;

let gen_textures (c : t) count =
    Array.init count (fun _ ->
        c##createTexture
    )
;;

let delete_textures (c : t) arr =
    Array.iter (fun tex ->
        c##deleteTexture tex
    ) arr
;;

let uniform4fv (c : t) loc (values : Buffer.Float.t) =
    c##uniform4fv_typed loc values
;;

let temp_array = Js.array [|0.; 0.|]
let uniform2fv (c : t) loc values =
    let v1 = Buffer.Float.get values 0 in
    let v2 = Buffer.Float.get values 1 in
    Js.array_set temp_array 0 v1;
    Js.array_set temp_array 1 v2;
    c##uniform2fv loc temp_array
;;

let bind_buffer (c : t) b (v : buffer_id) =
    c##bindBuffer b v

let draw_arrays (c : t) t o a = c##drawArrays t o a
let generate_mipmap (c : t) m = c##generateMipmap m
let color_mask (c : t) a b z d =
    let a = Js.bool a
    and b = Js.bool b
    and z = Js.bool z
    and d = Js.bool d in
    c##colorMask a b z d
;;

type program = WebGL.program Js.t
let use_program (c : t) p = c##useProgram p

let uniform1i (c : t) idx v = c##uniform1i idx v
let get_uniform_location (c : t) p name =
    c##getUniformLocation p (Js.string name)

let finish (c : t) = c##finish

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
}

open Graphv_core_lib

let create_shader (c : t) vshader fshader =
    Utils.with_return (fun r ->
        let prog = c##createProgram in
        let vert = c##createShader c##._VERTEX_SHADER_ in
        let frag = c##createShader c##._FRAGMENT_SHADER_ in
        c##shaderSource vert (Js.string vshader);
        c##shaderSource frag (Js.string fshader);

        c##compileShader vert;
        let status = c##getShaderParameter vert c##._COMPILE_STATUS_ in
        if not Js.(to_bool status) then (
            print_endline "failed to compile vertex shader";
            r.return None
        );

        c##compileShader frag;
        let status = c##getShaderParameter frag c##._COMPILE_STATUS_ in
        if not Js.(to_bool status) then (
            print_endline "failed to compile fragment shader";
            r.return None
        );

        c##attachShader prog vert;
        c##attachShader prog frag;

        c##bindAttribLocation prog 0 (Js.string "vertex");
        c##bindAttribLocation prog 1 (Js.string "tcoord");

        c##linkProgram prog;
        let status = c##getProgramParameter prog c##._LINK_STATUS_ in
        if not Js.(to_bool status) then (
            print_endline "Failed to link program";
            r.return None;
        );

        Some prog
    )
;;

let create_program (c : t) =
    match create_shader c Gles2_shaders.fill_vert Gles2_shaders.fill_frag with
    | None -> None
    | Some prog ->
        let view_size = c##getUniformLocation prog Js.(string "viewSize") in
        let tex = c##getUniformLocation prog Js.(string "tex") in
        let frag = c##getUniformLocation prog Js.(string "frag") in
        let vert_buf = c##createBuffer in
        Some (prog, { frag; tex; view_size; vert_buf})
;;
