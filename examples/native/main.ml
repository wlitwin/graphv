open Tgles2

module NVG = Graphv_gles2_native

module GLFWExtras = struct
    open Ctypes
    open Foreign

    let glfwSetErrorCallback : (int -> string -> unit) -> (int -> string -> unit) =
        let errorfun = int @-> string @-> returning void in
        foreign "glfwSetErrorCallback" (funptr errorfun @-> returning (funptr errorfun))
    ;;
end

let errorcb error desc =
    Printf.printf "GLFW error %d: %s\n%!" error desc


let rec check_error str =
    let err = Gl.get_error() in
    if err <> Gl.no_error then (
        Printf.printf "Error %08x after %s\n%!" err str;
        check_error str
    )
;;

let load_data vg =
    let _ = NVG.Text.create vg ~name:"mono" ~file:"./examples/assets/mono.ttf" in
    let _ = NVG.Text.create vg ~name:"icons" ~file:"./examples/assets/entypo.ttf" in
    let _ = NVG.Text.create vg ~name:"sans" ~file:"./examples/assets/Roboto-Regular.ttf" in
    let _ = NVG.Text.create vg ~name:"sans-bold" ~file:"./examples/assets/Roboto-Bold.ttf" in
    let _ = NVG.Text.create vg ~name:"emoji" ~file:"./examples/assets/NotoEmoji-Regular.ttf" in
    NVG.Text.add_fallback vg ~name:"sans" ~fallback:"emoji";
    NVG.Text.add_fallback vg ~name:"sans-bold" ~fallback:"emoji";
    NVG.Text.set_font_face vg ~name:"mono";

    let images = Array.make 12 NVG.Image.dummy in
    for i=0 to 11 do
        let file = Printf.sprintf "./examples/assets/images/image%d.jpg" (i+1) in
        match Stb_image.load ~channels:3 file with 
        | Error (`Msg e) -> failwith e
        | Ok img -> 
            let len = Bigarray.Array1.dim img.data in
            let arr = Array.make (len/3) NVG.Color.transparent in
            for i=0 to (len/3) - 1 do
                let r = Bigarray.Array1.get img.data (i*3+0) in
                let g = Bigarray.Array1.get img.data (i*3+1) in
                let b = Bigarray.Array1.get img.data (i*3+2) in
                let a = 255 in
                arr.(i) <- NVG.Color.rgba ~r ~g ~b ~a;
            done;
            let img = NVG.Image.from_color vg ~data:arr ~flags:NVG.ImageFlags.no_flags 
                ~width:img.width
                ~height:img.height
            in
            let img = match img with None -> failwith "no image" | Some img -> img in
            images.(i) <- img;
    done;
    Demo.{ images }
;;

let _ =
    GLFW.init();
    at_exit GLFW.terminate;
    let _res = GLFWExtras.glfwSetErrorCallback errorcb in
    GLFW.windowHint ~hint:GLFW.ClientApi ~value:GLFW.OpenGLESApi;
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:2;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:0;

    let window = GLFW.createWindow ~width:1000 ~height:600 ~title:"window" () in
    (* Make the window's context current *)
    GLFW.makeContextCurrent ~window:(Some window);
    GLFW.swapInterval ~interval:0;

    Gl.clear_color 0.3 0.3 0.32 1.;

    Memtrace.trace_if_requested();

    let ctx = NVG.create ~flags:NVG.CreateFlags.(antialias lor stencil_strokes) () in

    let graph = PerfGraph.init PerfGraph.FPS "Frame Time" in
    let t = GLFW.getTime() |> ref in
    let data = load_data ctx in
    let continue = ref true in
    let min_fps = ref Float.max_float in
    let max_fps = ref Float.min_float in
    let blowup = ref false in
    GLFW.setKeyCallback ~window ~f:(Some (fun _ key _ state _ ->
        match key, state with
        | GLFW.Space, GLFW.Release -> blowup := not !blowup
        | _ -> ()
    )) |> ignore;

    while not GLFW.(windowShouldClose ~window) && !continue do

        let now = GLFW.getTime() in
        let dt = now -. !t in
        t := now;

        PerfGraph.update graph dt;

        if now > 2. then (
            let avg = 1. /. PerfGraph.average graph in
            min_fps := Float.min avg !min_fps;
            max_fps := Float.max avg !max_fps;
        );

        let mx, my = GLFW.getCursorPos ~window in
        let win_w, win_h = GLFW.getWindowSize ~window in
        
        Gl.viewport 0 0 win_w win_h;
        Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit lor Gl.stencil_buffer_bit);

        Gl.enable Gl.blend;
        Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
        Gl.enable Gl.cull_face_enum;
        Gl.disable Gl.depth_test;

        let win_w, win_h = float win_w, float win_h in
        NVG.begin_frame ctx ~width:win_w ~height:win_h ~device_ratio:1.;

        PerfGraph.render graph ctx 5. 5.;
        Demo.render_demo ctx mx my win_w win_h now !blowup data;

        NVG.end_frame ctx;

        Gc.major_slice 0 |> ignore;

        GLFW.swapBuffers ~window;
        GLFW.pollEvents();

        (*continue := false;*)
    done;

    Printf.printf "MIN %.2f\n" !min_fps;
    Printf.printf "MAX %.2f\n%!" !max_fps;
    
    if Array.length Sys.argv = 1 then (
        while not GLFW.(windowShouldClose ~window) do
            GLFW.pollEvents();
            Unix.sleepf 0.25
        done
    )


