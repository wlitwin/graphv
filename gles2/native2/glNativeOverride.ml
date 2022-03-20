let create_program (_t : unit) =
    let shader = Gl_utils.create_shaders() in
    match shader with
    | None -> None
    | Some shader ->
        Some (shader.prog, Stubs.{
            frag = Hashtbl.find shader.locs "frag";
            tex = Hashtbl.find shader.locs "tex";
            view_size = Hashtbl.find shader.locs "viewSize";
            vert_buf = Gl_utils.gen_buffers();
        })
    ;;

open Stubs

let buffer_data (_t : unit) target buffer size how =
    buffer_data target buffer size how
;;

