open Tgles2

module Gv = Graphv_gles2_native

type rect = {
    mutable x : float;
    mutable y : float;
    mutable w : float;
    mutable h : float;
    mutable vx : float;
    mutable vy : float;
}

let _ =
    GLFW.init();
    at_exit GLFW.terminate;
    GLFW.windowHint ~hint:GLFW.ClientApi ~value:GLFW.OpenGLESApi;
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:2;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:0;

    let window =
        GLFW.createWindow ~width:400 ~height:400 ~title:"window" ()
    in

    GLFW.makeContextCurrent ~window:(Some window);
    GLFW.swapInterval ~interval:0;

    Gl.clear_color 0.3 0.3 0.32 1.;

    let vg = Gv.create
        ~flags:Gv.CreateFlags.(no_flags)
        ()
    in

    Random.self_init();

    let vel = 400. in
    let vel_2 = ~-.(vel *. 0.5) in

    let rects = Array.init (15000/3) (fun _ ->
        {
            x = Random.float 200. +. 100.;
            y = Random.float 200. +. 100.;
            w = 20.;
            h = 20.;
            vx = Random.float vel +. vel_2;
            vy = Random.float vel +. vel_2;
        }
    ) in

    let time = ref (GLFW.getTime()) in
    let frames = ref 0 in
    let last_count = ref !time in

    while not GLFW.(windowShouldClose ~window) do
        let win_w, win_h = GLFW.getWindowSize ~window in
        Gl.viewport 0 0 win_w win_h;
        Gl.clear (
            Gl.color_buffer_bit
            lor Gl.depth_buffer_bit
            lor Gl.stencil_buffer_bit
        );

        let now = GLFW.getTime() in
        let dt = now -. !time in
        time := now;

        if now -. !last_count >= 1. then (
            last_count := now;
            Printf.printf "FPS %d\n%!" !frames;
            frames := 0;
        );

        incr frames;

        Gv.begin_frame vg
            ~width:(float win_w)
            ~height:(float win_h)
            ~device_ratio:1.
            ;

        let open Gv in

        let win_w = float win_w in
        let win_h = float win_h in

        let len = Array.length rects in
        for i=0 to len-1 do
            let r = rects.(i) in
            r.x <- r.x +. r.vx*.dt;
            r.y <- r.y +. r.vy*.dt;

            if r.x +. r.w > win_w then (
                r.x <- win_w -. r.w;
                r.vx <- ~-.(r.vx);
            );

            if r.y +. r.h > win_h then (
                r.y <- win_h -. r.h;
                r.vy <- ~-.(r.vy);
            );

            if r.x < 0. then (
                r.x <- 0.;
                r.vx <- ~-.(r.vx);
            );

            if r.y < 0. then (
                r.y <- 0.;
                r.vy <- ~-.(r.vy);
            );

            Path.begin_ vg;
            (*Path.rect vg ~x:r.x ~y:r.y ~w:r.w ~h:r.h;*)
            Path.circle vg ~cx:r.x ~cy:r.y ~r:(r.w*.0.5);
            let r1 = Float.abs r.vx /. 400. *. 255. |> int_of_float in
            let g1 = Float.abs r.vy /. 400. *. 255. |> int_of_float in
            set_fill_color vg
                ~color:Gv.Color.(rgba ~r:r1 ~g:g1 ~b:255 ~a:255);
            fill vg;
        done;


        (*
        Gv.Transform.scale vg ~x:2. ~y:2.;

        Gv.Path.begin_ vg;
        Gv.Path.arc vg ~cx:200. ~cy:200. ~a0:0. ~a1:(Float.pi*.0.5) ~r:100. ~dir:Gv.Winding.CW;
        Gv.set_stroke_color vg
            ~color:Gv.Color.(rgba ~r:255 ~g:203 ~b:255 ~a:255);
        Gv.set_stroke_width vg ~width:50.;
        Gv.stroke vg;

        Gv.Path.begin_ vg;
        Gv.Path.circle vg ~cx:300. ~cy:200. ~r:30.;
        Gv.set_fill_color vg
            ~color:Gv.Color.(rgba ~r:154 ~g:203 ~b:255 ~a:255);
        Gv.fill vg;
        *)

        Gv.end_frame vg;

        GLFW.swapBuffers ~window;
        GLFW.pollEvents();
    done;
;;
