module Gl = Stubs

let debug = true

let rec check_error str =
    if debug then (
        let err = Gl.get_error() in
        if err <> Gl.no_error then (
            Printf.printf "Error %08x after %s\n%!" err str;
            check_error str
        )
    )
;;

let dump_gl_error item name dump_fn error_str =
    (*
    let len = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
    let str = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 512 in
    dump_fn item 512 (Some len) str;
    let len = Bigarray.Array1.get len 0 |> Int32.to_int in
    let byte = Bytes.create len in
    for i=0 to len-1 do
        Bytes.set byte i (Bigarray.Array1.get str i)
    done;
    *)
    let str = dump_fn item in
    Printf.printf error_str name str
;;

let dump_shader_error shader name =
    dump_gl_error shader name Gl.get_shader_info_log "%s shader error %s\n%!"
;;

let dump_program_error program name =
    dump_gl_error program name Gl.get_program_info_log "%s error %s\n%!"
;;

let gen_buffers () =
    let bufs = [|0|] in
    Gl.gen_buffers bufs;
    bufs.(0)
;;

module Shader = struct
    type t = {
        prog : int;
        frag : int;
        vert : int;
        locs : (string, int) Hashtbl.t;
    }
end

let create_shader vshader fshader =
    Graphv_core_lib.Utils.with_return (fun r ->
        let prog = Gl.create_program() in
        let vert = Gl.create_shader Gl.vertex_shader in
        let frag = Gl.create_shader Gl.fragment_shader in
        Gl.shader_source vert vshader;
        Gl.shader_source frag fshader;

        check_error "shader source";

        Gl.compile_shader vert;
        check_error "compile vert";
        if Gl.get_shaderiv vert Gl.compile_status <> Gl.true_ then (
            dump_shader_error vert "vert";
            r.return None
        );

        Gl.compile_shader frag;
        check_error "compile frag";
        if Gl.get_shaderiv frag Gl.compile_status <> Gl.true_ then (
            dump_shader_error vert "frag";
            r.return None
        );

        Gl.attach_shader prog vert;
        Gl.attach_shader prog frag;
        check_error "attach shaders";

        Gl.bind_attrib_location prog 0 "vertex";
        Gl.bind_attrib_location prog 1 "tcoord";
        check_error "bind attributes";

        Gl.link_program prog;
        check_error "link program";
        if Gl.get_programiv prog Gl.link_status <> Gl.true_ then (
            dump_program_error prog "program";
            r.return None
        );

        check_error "end create shader";

        Some Shader.{
            prog;
            vert;
            frag;
            locs = Hashtbl.create 10;
        }
    )
;;

let create_shaders () =
    let shader = create_shader Gles2_shaders.fill_vert Gles2_shaders.fill_frag in
    match shader with
    | None -> None
    | Some shader ->
        check_error "uniforms";
        Hashtbl.replace shader.locs "viewSize" (Gl.get_uniform_location shader.prog "viewSize");
        Hashtbl.replace shader.locs "tex" (Gl.get_uniform_location shader.prog "tex");
        Hashtbl.replace shader.locs "frag" (Gl.get_uniform_location shader.prog "frag");
        Some shader

;;

let delete_shader (shader : Shader.t) =
    if shader.prog <> 0 then (
        Gl.delete_program shader.prog
    );
    if shader.vert <> 0 then (
        Gl.delete_shader shader.vert
    );
    if shader.frag <> 0 then (
        Gl.delete_shader shader.frag
    );
;;

