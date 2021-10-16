open Tgles2

module NVG = Graphv_gles2_native

type shape = {
    mutable x : float;
    mutable y : float;
    mutable sx : float;
    mutable sy : float;
    mutable opacity : float;
    mutable rotate : float;
}

let opacity shape s e t =
    shape.opacity <- Anim.lerp s e t
;;

let sx shape s e t =
    shape.sx <- Anim.lerp s e t
;;

let sy shape s e t =
    shape.sy <- Anim.lerp s e t
;;

let x shape s e t =
    shape.x <- Anim.lerp s e t

let y shape s e t =
    shape.y <- Anim.lerp s e t

let rotate shape s e t =
    shape.rotate <- Anim.lerp s e t

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
    GLFW.swapInterval ~interval:1;

    Gl.clear_color 0.3 0.3 0.32 1.;

    let vg = NVG.create
        ~flags:NVG.CreateFlags.(antialias lor stencil_strokes) 
        () 
    in

    let rect = {
        x = 400.;
        y = 400.;
        sx = 1.;
        sy = 1.;
        opacity = 0.1;
        rotate = 0.;
    } in

    let ease = Anim.Ease.linear in
    let anim = Anim.(
        parallel ~complete:(fun _ _ ->
            Printf.printf "CALLED PARALLEL COMPLETE\n%!";
        ) [
            serial ~complete:(fun _ _ -> 
                Printf.printf "CALLED SERIAL COMPLETE\n%!";
                (*
                rect.x <- 400.;
                rect.y <- 400.;
                rect.sx <- 1.;
                rect.sy <- 1.;
                rect.opacity <- 0.1;
                rect.rotate <- 0.;
                *)
            ) ~direction:(Mirror Backward) ~repeat:(Count 2) [
                create 1. ~ease (x rect 0. 200.);
                create 1. ~ease (y rect 50. 100.);
                create 1. ~ease (x rect 200. 400.);
                create 1. ~ease (y rect 100. 400.);
            ];
            (*
            create ~ease:Ease.out_cubic 4. (opacity rect 0.1 1.);
            create ~ease 1. (rotate rect 0. (4.*.Float.pi*.2.));
            serial [
                create ~ease ~delay:1. 1. (sx rect 1. 5.);
                create ~ease ~delay:0. 1. (sy rect 1. 5.);
            ];
            *)
        ]
    )
    in

    let circles = [|
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 1.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 1.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 1.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 1.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 1.; rotate = 0. };
    |] in

    let loc_xy circle t =
        circle.x <- (Float.cos (t*.2.*.Float.pi))*.50. +. 650.;
        circle.y <- (Float.sin (t*.2.*.Float.pi))*.50. +. 100.;
    in

    let ease = Anim.Ease.in_out_quint in
    (*
    let anim = Anim.Anim.(
        parallel ~repeat:(Count 2) ~complete:(fun _ _ -> print_endline "Circles done") [
            anim;
            create ~delay:0.0 2.5 ~ease (loc_xy circles.(0));
            create ~delay:0.2 2.5 ~ease (loc_xy circles.(1));
            create ~delay:0.4 2.5 ~ease (loc_xy circles.(2));
            create ~delay:0.6 2.5 ~ease (loc_xy circles.(3));
            create ~delay:0.8 2.5 ~ease (loc_xy circles.(4));
        ]
    ) in
*)

    let x = ref 0. in

    let ex = Anim.(
        create ~direction:(Mirror Forward) ~ease:Anim.Ease.out_cubic ~repeat:Infinite 0.5 (fun t -> x := Anim.lerp 0. 200. t)
    ) in
    
    let driver = Anim.Driver.create() in
    (*Anim.Anim.Driver.start_ driver ex;*)
    Anim.Driver.start_ driver anim;

    (*exit 1 |> ignore;*)

    let time = GLFW.getTime() |> ref in

    let timer = ref 0. in

    GLFW.setKeyCallback ~window ~f:(Some (fun _win key _ action _mods ->
        match action, key with
        | Release, Space -> 
                timer := 0.;
                rect.x <- 0.;
                rect.y <- 50.;
                rect.sx <- 1.;
                rect.sy <- 1.;
                rect.opacity <- 0.1;
                rect.rotate <- 0.;
                for i=0 to 4 do
                    let c = circles.(i) in
                    c.x <- 700.;
                    c.y <- 100.;
                done
        | _ -> ()
    )) |> ignore;

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

        Anim.Driver.tick driver dt;
        timer := !timer +. dt;

        NVG.begin_frame vg
            ~width:(float win_w)
            ~height:(float win_h)
            ~device_ratio:1.
            ;

        for i=4 downto 0 do
            NVG.Path.begin_ vg;
            let c = circles.(i) in
            NVG.Path.circle vg ~cx:c.x ~cy:c.y ~r:10.;
            NVG.fill vg;
            NVG.stroke vg;
        done;

        NVG.Path.begin_ vg;
        NVG.Path.rect vg ~x:!x ~y:250. ~w:20. ~h:20.;
        NVG.fill vg;


        NVG.Path.begin_ vg;

        NVG.Transform.translate vg ~x:(rect.x +. 25.*.rect.sx) ~y:(rect.y +. 25.*.rect.sy);
        NVG.Transform.rotate vg ~angle:rect.rotate;
        NVG.Transform.translate vg ~x:(~-.25.*.rect.sx) ~y:(~-.25.*.rect.sy);
        (*NVG.Transform.translate vg ~x:(rect.x) ~y:(rect.y);
        NVG.Transform.rotate vg ~angle:rect.rotate;
        NVG.Transform.translate vg ~x:(rect.sx) ~y:(rect.sy);*)

        NVG.Path.rect vg ~x:(0.) ~y:(0.) ~w:(50.*.rect.sx) ~h:(50.*.rect.sy);
        (*NVG.Path.circle vg ~cx:0. ~cy:0. ~r:(25.*.rect.sx);
        let paint = NVG.Paint.box_gradient vg
            ~x:0. ~y:0.
            ~w:(50.*.rect.sy)
            ~h:(50.*.rect.sx)
            ~r:10.
            ~f:20.
            ~icol:NVG.Color.white
            ~ocol:NVG.Color.black
        in
        NVG.set_fill_paint vg ~paint;
        *)

        NVG.set_fill_color vg 
            ~color:NVG.Color.(rgba ~r:255 ~g:255 ~b:255 ~a:(rect.opacity *. 255. |> int_of_float));
        NVG.fill vg;
        NVG.stroke vg;

        NVG.end_frame vg;

        GLFW.swapBuffers ~window;
        GLFW.pollEvents();
    done;
;;
