open Tgles2

module Gv = Graphv_gles2
module Anim = Graphv_anim

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

    let vg = Gv.create
        ~flags:Gv.CreateFlags.(antialias lor stencil_strokes) 
        () 
    in

    let rect = {
        x = 0.;
        y = 50.;
        sx = 1.;
        sy = 1.;
        opacity = 0.0;
        rotate = 0.;
    } in

    let ease = Anim.Ease.out_bounce in
    let anim = Anim.(
        parallel ~direction:(Mirror Forward) ~repeat:(Count 2) ~complete:(fun _ _ ->
            Printf.printf "CALLED PARALLEL COMPLETE\n%!";
        ) [
            serial ~complete:(fun _ _ -> 
                Printf.printf "CALLED SERIAL COMPLETE\n%!";
            ) [
                create 1. ~ease (x rect 0. 200.);
                create 1. ~ease (y rect 50. 100.);
                create 1. ~ease (x rect 200. 400.);
                create 1. ~ease (y rect 100. 400.);
            ];
            serial [
                create ~ease ~delay:1. 1. (sx rect 1. 5.);
                create ~ease ~delay:0. 1. (sy rect 1. 5.);
            ];
            create ~ease:Ease.out_cubic 4. (opacity rect 0.0 1.);
            create ~ease 1. (rotate rect 0. (1.*.Float.pi*.2.));
        ]
    )
    in

    let circles = [|
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 0.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 0.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 0.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 0.; rotate = 0. };
        { x = 700.; y = 100.; sx = 1.; sy = 1.; opacity = 0.; rotate = 0. };
    |] in

    let loc_xy circle t =
        circle.x <- (Float.cos (t*.2.*.Float.pi))*.50. +. 650.;
        circle.y <- (Float.sin (t*.2.*.Float.pi))*.50. +. 100.;
    in

    let ease = Anim.Ease.in_out_cubic in
    let circle_anim idx delay = Anim.(
            parallel [
                create ~delay 2.5 ~ease (loc_xy circles.(idx));
                serial ~delay [
                    create 1.25 ~ease (opacity circles.(idx) 0. 1.);
                    create 1.25 ~ease (opacity circles.(idx) 1. 0.);
                ];
            ];
        )
    in

    let ease = Anim.Ease.in_out_quint in
    let anim = Anim.(
        parallel ~ease ~complete:(fun _ _ -> print_endline "Circles done") [
            anim;
            parallel ~repeat:(Count 2) [
                circle_anim 0 0.;
                circle_anim 1 0.2;
                circle_anim 2 0.4;
                circle_anim 3 0.6;
                circle_anim 4 0.8;
            ]
        ]
    ) in

    let x = ref 0. in

    let ex = Anim.(
        create ~direction:(Mirror Forward) ~ease:Anim.Ease.out_cubic ~repeat:Infinite 0.5 (fun t -> x := Anim.lerp 0. 200. t)
    ) in
    
    let driver = Anim.Driver.create() in
    Anim.Driver.start_ driver anim;
    Anim.Driver.start_ driver ex;

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

        Gv.begin_frame vg
            ~width:(float win_w)
            ~height:(float win_h)
            ~device_ratio:1.
            ;

        for i=4 downto 0 do
            Gv.Path.begin_ vg;
            let c = circles.(i) in
            Gv.Path.circle vg ~cx:c.x ~cy:c.y ~r:10.;
            let color =
                Gv.Color.(rgba ~r:255 ~g:255 ~b:255 ~a:(c.opacity *. 255. |> int_of_float))
            in
            Gv.set_fill_color vg ~color;
            let color =
                Gv.Color.(rgba ~r:0 ~g:0 ~b:0 ~a:(c.opacity *. 255. |> int_of_float))
            in
            Gv.set_stroke_color vg ~color;
            Gv.fill vg;
            Gv.stroke vg;
        done;

        Gv.Path.begin_ vg;
        Gv.Path.rect vg ~x:!x ~y:250. ~w:20. ~h:20.;
        Gv.set_fill_color vg ~color:Gv.Color.white;
        Gv.set_stroke_color vg ~color:Gv.Color.black;
        Gv.fill vg;

        Gv.Path.begin_ vg;

        Gv.Transform.translate vg ~x:(rect.x +. 25.*.rect.sx) ~y:(rect.y +. 25.*.rect.sy);
        Gv.Transform.rotate vg ~angle:rect.rotate;
        Gv.Transform.translate vg ~x:(~-.25.*.rect.sx) ~y:(~-.25.*.rect.sy);
        (*Gv.Transform.translate vg ~x:(rect.x) ~y:(rect.y);
        Gv.Transform.rotate vg ~angle:rect.rotate;
        Gv.Transform.translate vg ~x:(rect.sx) ~y:(rect.sy);*)

        Gv.Path.rect vg ~x:(0.) ~y:(0.) ~w:(50.*.rect.sx) ~h:(50.*.rect.sy);
        (*Gv.Path.circle vg ~cx:0. ~cy:0. ~r:(25.*.rect.sx);
        let paint = Gv.Paint.box_gradient vg
            ~x:0. ~y:0.
            ~w:(50.*.rect.sy)
            ~h:(50.*.rect.sx)
            ~r:10.
            ~f:20.
            ~icol:Gv.Color.white
            ~ocol:Gv.Color.black
        in
        Gv.set_fill_paint vg ~paint;
        *)

        Gv.set_fill_color vg 
            ~color:Gv.Color.(rgba ~r:255 ~g:255 ~b:255 ~a:(rect.opacity *. 255. |> int_of_float));
        Gv.fill vg;
        Gv.stroke vg;

        Gv.Text.set_size vg ~size:14.;

        Gv.end_frame vg;

        GLFW.swapBuffers ~window;
        GLFW.pollEvents();
    done;
;;
