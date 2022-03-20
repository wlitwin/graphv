open Js_of_ocaml

include Graphv_core

let create ~flags ctx =

    let font_canvas : Dom_html.canvasElement Js.t =
        let c : Dom_html.canvasElement Js.t =
            Js.Unsafe.coerce (Dom_html.document##createElement Js.(string "canvas"))
        in
        c##.width := 2048;
        c##.height := 2048;
        c
    in

    let ctx_2d =
        let ctx = font_canvas##getContext Dom_html._2d_ in
        (Js.Unsafe.coerce ctx)##.imageSmoothingEnabled := Js._false;
        ctx
    in

    let gles = Gles.create ~flags ctx |> opt_exn in
    let font = Fontstash.create ctx_2d in
    create ~flags gles font
