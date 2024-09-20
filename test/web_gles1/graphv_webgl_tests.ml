open Js_of_ocaml
module Gv = Graphv_webgl

(* This scales the canvas to match the DPI of the window,
   it prevents blurriness when rendering to the canvas *)
let scale_canvas (canvas : Dom_html.canvasElement Js.t) =
    let dpr = Js.to_float Dom_html.window##.devicePixelRatio in
    let rect = canvas##getBoundingClientRect in
    let width = Js.to_float rect##.right -. Js.to_float rect##.left in
    let height = Js.to_float rect##.bottom -. Js.to_float rect##.top in
    canvas##.width := width *. dpr |> int_of_float;
    canvas##.height := height *. dpr |> int_of_float;
    let width = Printf.sprintf "%dpx" (int_of_float width) |> Js.string in
    let height = Printf.sprintf "%dpx" (int_of_float height) |> Js.string in
    canvas##.style##.width := width;
    canvas##.style##.height := height;
;;

type rect = {
    mutable x : float;
    mutable y : float;
    mutable w : float;
    mutable h : float;
    mutable vx : float;
    mutable vy : float;
}

let _ =
    let canvas = Js.Unsafe.coerce (Dom_html.getElementById_exn "canvas") in
    scale_canvas canvas;

    let webgl_ctx = 
        (* Graphv requires a stencil buffer to work properly *)
        let attrs = WebGL.defaultContextAttributes in
        attrs##.stencil := Js._true;
        match WebGL.getContextWithAttributes canvas attrs 
              |> Js.Opt.to_option 
        with
        | None -> 
            print_endline "Sorry your browser does not support WebGL";
            raise Exit
        | Some ctx -> ctx
    in

    webgl_ctx##clearColor
      (Js.float 0.3) (Js.float 0.3) (Js.float 0.32) (Js.float 1.);

    let vg = Gv.create
        ~flags:Gv.CreateFlags.(antialias lor stencil_strokes)
        webgl_ctx
    in

    Random.self_init();

    let vel = 400. in
    let vel_2 = ~-.(vel *. 0.5) in

    let rects = Array.init (4000) (fun _ ->
        {
            x = Random.float 200. +. 100.;
            y = Random.float 200. +. 100.;
            w = 20.;
            h = 20.;
            vx = Random.float vel +. vel_2;
            vy = Random.float vel +. vel_2;
        }
    ) in

    let time = ref 0. in
    let frames = ref 0 in
    let last_count = ref !time in

    let last_fps = ref "0" in

    Gv.Cache.begin_ vg;
    Gv.Path.rect vg ~x:0. ~y:0. ~w:20. ~h:20.;

    let tess = Gv.Cache.save vg Gv.Cache.Fill in

    let is_chrome : bool = Js.Unsafe.js_expr "window.chrome !== undefined" in

    Gv.Text.create vg ~name:"sans" ~file:"arial" |> ignore;

    let rec render now =
        let device_ratio = Js.to_float Dom_html.window##.devicePixelRatio in
        webgl_ctx##clear (
            webgl_ctx##._COLOR_BUFFER_BIT_ 
            lor webgl_ctx##._DEPTH_BUFFER_BIT_ 
            lor webgl_ctx##._STENCIL_BUFFER_BIT_
        );

        let now = Js.to_float now /. 1000. in
        let dt = now -. !time in
        time := now;

        if now -. !last_count >= 1. then (
            last_count := now;
            last_fps := Printf.sprintf "%d" !frames;
            frames := 0;
        );

        incr frames;

        Gv.begin_frame vg 
            ~width:(canvas##.width) 
            ~height:(canvas##.height) 
            ~device_ratio
            ;

        let open Gv in

        let win_w = float canvas##.width in
        let win_h = float canvas##.height in

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
                r.vx <- ~-.(r.vx) +. Random.float 200. -. 100.;
            );

            if r.y < 0. then (
                r.y <- 0.;
                r.vy <- ~-.(r.vy) +. Random.float 200. -. 100.;
            );

            let r1 = Float.abs r.vx /. 400. *. 255. |> int_of_float in
            let g1 = Float.abs r.vy /. 400. *. 255. |> int_of_float in
            set_fill_color vg
                ~color:Gv.Color.(rgba ~r:r1 ~g:g1 ~b:255 ~a:255);

            if is_chrome then (
                Gv.Path.begin_ vg;
                Gv.Path.rect vg ~x:r.x ~y:r.y ~w:r.w ~h:r.h;
                Gv.fill vg;
            ) else (
                Gv.Cache.begin_ vg;
                Gv.Cache.draw vg tess ~x:r.x ~y:r.y;
            );
        done;

        Text.set_size vg ~size:40.;
        Text.set_align vg ~align:Gv.Align.(left lor top);
        set_fill_color vg ~color:Color.black;
        Text.text vg ~x:2. ~y:2. !last_fps;
        set_fill_color vg ~color:Color.white;
        Text.text vg ~x:0. ~y:0. !last_fps;

        Gv.end_frame vg;

        Dom_html.window##requestAnimationFrame (Js.wrap_callback render) 
        |> ignore;
    in

    render (Js.float 0.)
;;
