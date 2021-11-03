open Tgles2
open Tsdl

module NVG = Graphv_gles2_native

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
    let scale = 1. in
    let ow = 1000. in
    let oh = 600. in
    let w = 1000. *. scale in
    let h = 600. *. scale in

    match Sdl.init Sdl.Init.video with
    | Error (`Msg e) -> Sdl.log "Init error %s" e; exit 1
    | Ok () ->
      Sdl.gl_set_attribute Sdl.Gl.depth_size 24 |> ignore;
      Sdl.gl_set_attribute Sdl.Gl.stencil_size 8 |> ignore;
      match Sdl.create_window ~w:(int_of_float w) ~h:(int_of_float h) "Window"
                Sdl.Window.(opengl + allow_highdpi)
      with
      | Error (`Msg e) -> Sdl.log "Create error %s" e; exit 2
      | Ok win ->
        Sdl.gl_set_swap_interval 0 |> ignore;
        let win_w, win_h = Sdl.gl_get_drawable_size win in
        Sdl.log "Win size %dx%d OpenGL size %dx%d" (int_of_float w) (int_of_float h) win_w win_h;
        let sw = float win_w /. w in
        let sh = float win_h /. h in
        Sdl.gl_set_attribute Sdl.Gl.stencil_size 1 |> ignore;
        match Sdl.gl_create_context win with
        | Error (`Msg e) -> Sdl.log "Context error %s" e; exit 3
        | Ok gl ->
            Gl.clear_color 0.3 0.3 0.32 1.;

            Memtrace.trace_if_requested();

            let ctx = NVG.create ~flags:NVG.CreateFlags.(antialias lor stencil_strokes lor tesselate_afd) () in

            let graph = PerfGraph.init PerfGraph.FPS "Frame Time" in
            let t = (Sdl.get_ticks() |> Int32.to_float) /. 1000. |> ref in
            let data = load_data ctx in
            let continue = ref true in
            let min_fps = ref Float.max_float in
            let max_fps = ref Float.min_float in
            let blowup = ref false in

            let quit = Array.length Sys.argv > 1 in

            let start = ref 0. in
            let total = ref 0. in
            let frames = ref 0 in

            let evt = Sdl.Event.create() in
            let should_close = ref false in

            while not !should_close && !continue do
                let now = (Sdl.get_ticks() |> Int32.to_float) /. 1000. in
                let dt = now -. !t in
                t := now;

                let update = now in
                PerfGraph.update graph dt;

                if now > 2. then (
                    let avg = 1. /. PerfGraph.average graph in
                    min_fps := Float.min avg !min_fps;
                    max_fps := Float.max avg !max_fps;
                );

                (*
                let mx, my = GLFW.getCursorPos ~window in
                let mx = mx /. scale in
                let my = my /. scale in
                let win_w, win_h = GLFW.getWindowSize ~window in
                   *)

                Gl.viewport 0 0 win_w win_h;
                Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit lor Gl.stencil_buffer_bit);

                Gl.enable Gl.blend;
                Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
                Gl.enable Gl.cull_face_enum;
                Gl.disable Gl.depth_test;

                let win_w = float win_w in
                let win_h = float win_h in
                NVG.begin_frame ctx ~width:win_w ~height:win_h ~device_ratio:1.;

                NVG.Transform.scale ctx ~x:(scale *. sw) ~y:(scale *. sh);

                PerfGraph.render graph ctx 5. 5.;
                Demo.render_demo ctx 0. 0. ow oh now !blowup data;

                NVG.end_frame ctx;

                let time = (Sdl.get_ticks() |> Int32.to_float) /. 1000. -. update in
                total := !total +. time;
                incr frames;

                if now -. !start >= 1. then (
                    Printf.printf "FPS %d Total %.3fms Avg %.3fms\n%!"
                        !frames (!total*.1000.) ((!total*.1000.) /. float !frames)
                        ;
                    frames := 0;
                    start := now;
                    total := 0.;
                );

                (*Gc.major_slice 0 |> ignore;*)

                Sdl.gl_swap_window win;

                while Sdl.poll_event (Some evt) do
                  match Sdl.Event.enum (Sdl.Event.get evt Sdl.Event.typ) with
                  | `Quit -> should_close := true
                  | _ -> ()
                done;

                if quit then (
                    continue := now < 5.;
                )
            done;

            Printf.printf "MIN %.2f\n" !min_fps;
            Printf.printf "MAX %.2f\n%!" !max_fps;

            Sdl.gl_delete_context gl;
            Sdl.destroy_window win;
            Sdl.quit();
            
              (*
            if not quit then (
                while not GLFW.(windowShouldClose ~window) do
                    GLFW.pollEvents();
                    Unix.sleepf 0.25
                done
            )
                 *)


