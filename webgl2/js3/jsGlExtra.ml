open Js_of_ocaml
open Base
open GlTypes

let create_program (c : t) =
    match create_shader c Gles_shaders.fill_vert Gles_shaders.fill_frag with
    | None -> None
    | Some prog ->
        let view_size = c##getUniformLocation prog Js.(string "viewSize") in
        let tex = c##getUniformLocation prog Js.(string "tex") in
        let frag = c##getUniformLocation prog Js.(string "frag") in
        let vert_buf = c##createBuffer in
        let frag_buf = c##createBuffer in
        Some (prog, { frag; tex; view_size; vert_buf; frag_buf })
;;
