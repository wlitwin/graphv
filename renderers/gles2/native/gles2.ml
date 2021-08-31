include Stubs

type arg = unit
type t = unit
let create (t : t) : t = t

type locs = {
    frag : [`vec4] uniform_location;
    tex : int uniform_location;
    view_size : [`vec2] uniform_location;
    vert_buf : buffer_id;
}

let null_program = 0

let texture_equal (_t : t) (a : texture option) (b : texture option) : bool =
    match a, b with 
    | None, None -> true
    | Some a, Some b -> Int.equal a b
    | _ -> false
;;

(* These were faster when they were part of the Gles2.Impl 
   but were moved here to make WebGL/Js_of_ocaml faster because
   inlining works better when everything is in the same file.
 *)
module Dyn = Graphv_core_lib.Dyn.Make(Buffer)

let max (a : int) (b : int) : int =
    if a < b then b else a

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
        let len = num_verts t - 1 in
        let rec loop i =
            if (*i >= len*) len < i then ()
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

    let set t idx x y u v =
        t.size <- max (idx+1) t.size;
        check_size t idx;
        let off = idx*4 in
        Dyn.set t.arr off x;
        Dyn.set t.arr (off+1) y;
        Dyn.set t.arr (off+2) u;
        Dyn.set t.arr (off+3) v;
    ;;

    let get t idx =
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
    mutable winding : Graphv_core_lib.Winding.t;
    mutable convex : bool;
}

let empty_sub = VertexBuffer.Sub.create()

let create () = {
    first = 0;
    count = 0;
    closed = false;
    nbevel = 0;
    fill = empty_sub;
    stroke = empty_sub;
    winding = Graphv_core_lib.Winding.CCW;
    convex = true;
}

let reset (t : t) : unit =
    t.first <- 0;
    t.count <- 0;
    t.closed <- false;
    t.nbevel <- 0;
    t.fill <- empty_sub;
    t.stroke <- empty_sub;
    t.winding <- Graphv_core_lib.Winding.CCW;
    t.convex <- true;
end

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
        Some (shader.prog, {
            frag = Hashtbl.find shader.locs "frag";
            tex = Hashtbl.find shader.locs "tex";
            view_size = Hashtbl.find shader.locs "viewSize";
            vert_buf = Utils.gen_buffers();
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
