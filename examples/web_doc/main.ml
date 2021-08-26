open Js_of_ocaml
module NVG = Graphv_webgl

(* This scales the canvas to match the DPI of the window,
   it prevents blurriness when rendering to the canvas *)
let scale_canvas (canvas : Dom_html.canvasElement Js.t) =
    let dpr = Dom_html.window##.devicePixelRatio in
    let rect = canvas##getBoundingClientRect in
    let width = rect##.right -. rect##.left in
    let height = rect##.bottom -. rect##.top in
    canvas##.width := width *. dpr |> int_of_float;
    canvas##.height := height *. dpr |> int_of_float;
    let width = Printf.sprintf "%dpx" (int_of_float width) |> Js.string in
    let height = Printf.sprintf "%dpx" (int_of_float height) |> Js.string in
    canvas##.style##.width := width;
    canvas##.style##.height := height;
;;

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

    let open NVG in

    let vg = create 
        ~flags:CreateFlags.(antialias lor stencil_strokes) 
        webgl_ctx 
    in 

    (* File in this case is actually the CSS font name *)
    Text.create vg ~name:"sans" ~file:"sans" |> ignore;

    webgl_ctx##clearColor 0.3 0.3 0.32 1.;

    let rec render (time : float) =
        webgl_ctx##clear (
            webgl_ctx##._COLOR_BUFFER_BIT_ 
            lor webgl_ctx##._DEPTH_BUFFER_BIT_ 
            lor webgl_ctx##._STENCIL_BUFFER_BIT_
        );

        let device_ratio = Dom_html.window##.devicePixelRatio in
        begin_frame vg 
            ~width:(canvas##.width) 
            ~height:(canvas##.height) 
            ~device_ratio
            ;
        Transform.scale vg ~x:device_ratio ~y:device_ratio;

        Path.begin_ vg;
        Path.rect vg ~x:40. ~y:40. ~w:320. ~h:320.;
        set_fill_color vg ~color:Color.(rgba ~r:154 ~g:203 ~b:255 ~a:200);
        fill vg;

        Transform.translate vg ~x:200. ~y:200.;
        Transform.rotate vg ~angle:(time *. 0.0005);

        Text.set_font_face vg ~name:"sans";
        Text.set_size vg ~size:48.;
        Text.set_align vg ~align:Align.(center lor middle);
        set_fill_color vg ~color:Color.white;
        Text.text vg ~x:0. ~y:0. "Hello World!";

        NVG.end_frame vg;

        Dom_html.window##requestAnimationFrame (Js.wrap_callback render) 
        |> ignore;
    in

    Dom_html.window##requestAnimationFrame (Js.wrap_callback render) 
    |> ignore;
;;
