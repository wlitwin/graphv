open Js_of_ocaml
open! Graphv_core_lib

type t = WebGL.renderingContext Js.t
type arg = WebGL.renderingContext Js.t
let create (t : t) : t = t

module Buffer = struct
    module UByte = struct
        type t = Typed_array.uint8Array Js.t

        let set = Typed_array.set
        let get = Typed_array.unsafe_get
        let [@inline always] length (t : t) : int = t##.length
        let [@inline always] sub (t : t) (start : int) (len : int) : t = 
            t##subarray start len

        let create size =
            new%js Typed_array.uint8Array size

        let empty = create 0
    end

    module Float = struct
        type t = Typed_array.float32Array Js.t

        let set : t -> int -> float -> unit = Typed_array.set
        let get : t -> int -> float  = Typed_array.unsafe_get
        let [@inline always] length (t : t) : int = t##.length

        let fill (t : t) (value : float) =
            let len : int = Js.Unsafe.get t "length" - 1 in
            for i=0 to len do
                Typed_array.set t i value
            done

        let blit ~(src : t) ~(s_off : int) ~(dst : t) ~(d_off : int) ~(len : int) : unit =
            let len : int = len-1 in
            for i=0 to len do
                Typed_array.set dst (d_off+i) (Typed_array.unsafe_get src (s_off+i))
            done
        ;;

        let create size = new%js Typed_array.float32Array size
    end
end

module Dyn = struct
    type underlying = Buffer.Float.t

    type t = {
        mutable arr : Buffer.Float.t;
        mutable size : int;
    }

    let create size = {
        arr = Buffer.Float.create size;
        size = 0;
    }

    let unsafe_array (t : t) : underlying = t.arr
    let length (t : t) : int = t.size
    let capacity (t : t) : int = Buffer.Float.length t.arr
    let [@inline always] set (t : t) (i : int) (v : float) : unit = Buffer.Float.set t.arr i v
    let [@inline always] get (t : t) (i : int) : float = Buffer.Float.get t.arr i
    let [@inline always] clear (t : t) : unit =
        t.size <- 0

    let add_range t amount =
        let len = capacity t in
        if t.size + amount >= len then (
            let new_len = (max (t.size+amount) len)*3/2 in
            let new_arr = Buffer.Float.create new_len in
            Buffer.Float.blit ~src:t.arr ~s_off:0 ~dst:new_arr ~d_off:0 ~len:len;
            t.arr <- new_arr
        );
        let offset = t.size in
        t.size <- t.size + amount;
        offset
    ;;

    module Sub = struct
        type sub = {
            off : int;
            len : int;
            t : t;
        }

        let sub (t : t) off len = {
            off; len; t
        }

        let length t = t.len

        let offset t = t.off

        let get t idx =
            assert (t.off + idx < t.t.size);
            get t.t (t.off + idx)
        ;;

        let blit ~src ~dst ~(src_start : int) ~(dst_start : int) ~len =
            Buffer.Float.blit ~src:src.t.arr ~s_off:(src_start+src.off) ~dst:dst.arr ~d_off:dst_start ~len
        end
end

let [@inline always] max (a : int) (b : int) : int =
    if a < b then b else a
;;

module VertexBuffer = struct
    type t = {
        arr : Dyn.t;
        mutable size : int;
    }

    let create () =
        let arr = Dyn.create 1000 in
        {
            arr;
            size = 0;
        }
    ;;

    let clear t = 
        Dyn.clear t.arr;
        t.size <- 0;
    ;;

    let iteri t ~f = 
        for i=0 to t.size-1 do
            f i Dyn.(get t.arr i)
        done
    ;;

    let iter t ~f =
        for i=0 to t.size-1 do
            f Dyn.(get t.arr i)
        done
    ;;

    let num_verts t =
        t.size
    ;;

    let capacity t =
        Dyn.capacity t.arr / 4
    ;;

    let iterv t ~f =
        let len = num_verts t in
        let rec loop i =
            if i >= len then ()
            else (
                let x = Dyn.get t.arr (i*4+0) in
                let y = Dyn.get t.arr (i*4+1) in
                let u = Dyn.get t.arr (i*4+2) in
                let v = Dyn.get t.arr (i*4+3) in
                f x y u v;
                loop (i+1)
            )
        in
        loop 0
    ;;

    let [@inline always] check_size t idx =
        let len = (Dyn.length t.arr lsr 2) - 1 in
        if (*idx >= len*) len < idx then (
            let new_len = max (idx - len + 2) 1 in
            Dyn.add_range t.arr (new_len lsl 2) |> ignore
        );
    ;;

    let set (t : t) (idx : int) (x : float) (y : float) (u : float) (v : float) : unit =
        t.size <- max (idx+1) t.size;
        check_size t idx;
        let off = idx*4 in
        Dyn.set t.arr off x;
        Dyn.set t.arr (off+1) y;
        Dyn.set t.arr (off+2) u;
        Dyn.set t.arr (off+3) v;
    ;;

    let get (t : t) (idx : int) : float*float*float*float =
        let x = Dyn.get t.arr (idx*4+0) in
        let y = Dyn.get t.arr (idx*4+1) in
        let u = Dyn.get t.arr (idx*4+2) in
        let v = Dyn.get t.arr (idx*4+3) in
        x, y, u, v
    ;;

    let num_bytes t =
        t.size * 4 * 4
    ;;

    let num_floats t =
        t.size * 4
    ;;

    let unsafe_array t =
        Dyn.unsafe_array t.arr

    module Sub = struct
        type parent = t
        type t = Dyn.Sub.sub

        let sub p start end_ =
            Dyn.Sub.sub p.arr (start*4) (end_*4)
        ;;

        let vertex_offset (t : t) =
            Dyn.Sub.offset t / 4

        let length t = Dyn.Sub.length t / 4

        let blit ~src ~dst ~src_start ~dst_start ~len =
            (* Check if the dst is large enough... *)
            check_size dst (dst_start + len);
            dst.size <- max (dst.size+len) dst.size;
            Dyn.Sub.blit 
                ~src
                ~dst:dst.arr
                ~src_start:(src_start*4)
                ~dst_start:(dst_start*4)
                ~len:(len*4)
        ;;

        let num_verts t =
            Dyn.Sub.length t / 4

        let create () =
            Dyn.Sub.sub Dyn.(create 1) 0 1

        let empty = create()
    end
end
    
module Path = struct
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

    let empty_sub = VertexBuffer.Sub.empty

    let create () = {
        first = 0;
        count = 0;
        closed = false;
        nbevel = 0;
        fill = empty_sub;
        stroke = empty_sub;
        winding = Winding.CCW;
        convex = true;
    }
    
    let reset (t : t) : unit =
        t.first <- 0;
        t.count <- 0;
        t.closed <- false;
        t.nbevel <- 0;
        t.fill <- empty_sub;
        t.stroke <- empty_sub;
        t.winding <- Winding.CCW;
        t.convex <- true;
end

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

let buffer_data (c : t) t (buffer : buffer) b = 
    c##bufferData t buffer b

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
