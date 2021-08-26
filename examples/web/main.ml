open Js_of_ocaml

let rot = ref 0.

let graph = PerfGraph.init PerfGraph.FPS "Frame Time"

let mx = ref 0.
let my = ref 0.

let blowup = ref false

let raf vg data =
    let prev = ref 0. in
    let rec closure vg data now =
        let ctx_webgl = NVG.ctx_webgl in
        ctx_webgl##clearColor 0.3 0.3 0.32 1.;
        ctx_webgl##clear (ctx_webgl##._COLOR_BUFFER_BIT_ lor ctx_webgl##._DEPTH_BUFFER_BIT_ lor ctx_webgl##._STENCIL_BUFFER_BIT_);
        let w = NVG.canvas##.width in
        let h = NVG.canvas##.height in
        ctx_webgl##viewport 0 0 w h;

        NVG.begin_frame vg ~width:(float w) ~height:(float h) ~device_ratio:1.;

        let now = now /. 1000. in
        let dt = now -. !prev in
        prev := now;

        PerfGraph.update graph dt;

        Demo.render_demo vg !mx !my (float w) (float h) now !blowup data;
        PerfGraph.render graph vg 5. 5.;

        NVG.end_frame vg;

        Dom_html.window##requestAnimationFrame Js.(wrap_callback (closure vg data))
        |> ignore
    in
    closure vg data
;;

let load_data (vg : NVG.t) k =
    let img_canvas : Dom_html.canvasElement Js.t =
        Dom_html.document##createElement Js.(string "canvas")
        |> Js.Unsafe.coerce
    in
    let img_ctx = img_canvas##getContext Dom_html._2d_ in

    let images = Array.make 12 NVG.Image.dummy in

    let count = ref 0 in

    let add_image idx w h data =
        let data = new%js Typed_array.uint8Array_fromBuffer (Js.Unsafe.coerce data##.data) in
        let img = NVG.Image.from_buffer vg ~data ~flags:NVG.ImageFlags.no_flags
            ~width:w
            ~height:h
        in
        let img = match img with None -> failwith "img" | Some img -> img in
        images.(idx) <- img;
        incr count;
    in

    let load_image idx (id : string) src =
        let img : Dom_html.imageElement Js.t =
            Dom_html.getElementById id
            |> Js.Unsafe.coerce
        in
        img##.onload := Dom.handler (fun _ ->
            img_canvas##.width := img##.width;
            img_canvas##.height := img##.height;
            img_ctx##drawImage img 0. 0.;
            let w = img##.width in
            let h = img##.height in
            add_image idx w h (img_ctx##getImageData 0. 0. (float w) (float h));
            Js._true
        );
        img##.src := Js.string src
    in

    for i=0 to 11 do
        load_image i Printf.(sprintf "img%d" (i+1)) Printf.(sprintf "../assets/images/image%d.jpg" (i+1))
    done;

    let rec wait () =
        if !count < 12 then (
            Dom_html.setTimeout wait 100. |> ignore
        ) else (
            k Demo.{ images }
        )
    in

    wait ()
;;

let scale_canvas (canvas : Dom_html.canvasElement Js.t) (width : int) (height : int) =
    let dpr = Dom_html.window##.devicePixelRatio in
    canvas##.style##.width := Printf.sprintf "%.2fpx" (float width /. dpr) |> Js.string;
    canvas##.style##.height := Printf.sprintf "%.2fpx" (float height /. dpr) |> Js.string;
    canvas##.width := width;
    canvas##.height := height;
;;

let scale_canvas_by_body (canvas : Dom_html.canvasElement Js.t) =
    let body = Dom_html.document##.body in
    let dpr = Dom_html.window##.devicePixelRatio in
    let dpr = if dpr > 1.5 then 1.5 else dpr in
    let w = float body##.clientWidth *. dpr |> int_of_float in
    let h = float body##.clientHeight *. dpr |> int_of_float in
    scale_canvas canvas w h;
;;

let _ =
    let open NVG in
    let w, h = Js.Unsafe.(coerce canvas)##.width, Js.Unsafe.(coerce canvas)##.height in
    Dom_html.window##.onresize := Dom_html.handler (fun _evt ->
        scale_canvas_by_body canvas;
        Js._true
    );
    Dom_html.window##.onclick := Dom_html.handler (fun _ ->
        blowup := not !blowup;
        Js._false
    );
    ctx_webgl##viewport 0 0 w h;
    ctx_webgl##clearColor 1. 0.5 0. 1.;
    ctx_webgl##clear (ctx_webgl##._COLOR_BUFFER_BIT_ lor ctx_webgl##._DEPTH_BUFFER_BIT_ lor ctx_webgl##._STENCIL_BUFFER_BIT_);
    canvas##.onmousemove := Dom.handler (fun (evt : Dom_html.mouseEvent Js.t) ->
        mx := float (evt##.clientX) *. Dom_html.window##.devicePixelRatio;
        my := float (evt##.clientY) *. Dom_html.window##.devicePixelRatio;
        Js._false
    );
    let touch = Dom.Event.make "touchmove" in
    Dom_html.addEventListener canvas touch (Dom.handler (fun evt ->
        let lst = evt##.touches in
        begin match Js.array_get lst 0 |> Js.Optdef.to_option with
        | None -> ()
        | Some evt ->
            mx := float (evt##.clientX) *. Dom_html.window##.devicePixelRatio;
            my := float (evt##.clientY) *. Dom_html.window##.devicePixelRatio;
        end;
        if lst##.length > 1 then Js._true else Js._false
    )) Js._false |> ignore;
    let vg = NVG.create ~flags:NVG.CreateFlags.(antialias lor stencil_strokes) ctx_webgl in

    let _ = NVG.Text.create vg ~name:"mono" ~file:"mono" in
    let _ = NVG.Text.create vg ~name:"icons" ~file:"icons" in
    let _ = NVG.Text.create vg ~name:"arial" ~file:"arial" in
    let _ = NVG.Text.create vg ~name:"sans" ~file:"Roboto" in
    let _ = NVG.Text.create vg ~name:"sans-bold" ~file:"Roboto-Bold" in
    let _ = NVG.Text.create vg ~name:"emoji" ~file:"Emoji" in
    NVG.Text.add_fallback vg ~name:"sans" ~fallback:"emoji";
    NVG.Text.add_fallback vg ~name:"sans-bold" ~fallback:"emoji";

    Dom_html.document##.onkeyup := Dom.handler (fun (evt : Dom_html.keyboardEvent Js.t) ->
        if evt##.keyCode = 32 then (
            blowup := not !blowup
        );
        Js._false
    );

    load_data vg (fun data ->
        scale_canvas_by_body canvas;
        raf vg data 0.
    );
;;
