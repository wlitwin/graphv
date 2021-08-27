module WebGLRenderer = Graphv_webgl_impl

module Gles2 = Graphv_gles2.Make(WebGLRenderer)

open Js_of_ocaml
let font_canvas : Dom_html.canvasElement Js.t =
    let c : Dom_html.canvasElement Js.t =
        Js.Unsafe.coerce (Dom_html.document##createElement Js.(string "canvas"))
    in
    c##.width := 2048;
    c##.height := 2048;
    c
;;

let ctx_2d =
    let ctx = font_canvas##getContext Dom_html._2d_ in
    (Js.Unsafe.coerce ctx)##.imageSmoothingEnabled := Js._false;
    ctx
;;

module FontJS = Graphv_font_js.Backend.Impl(struct
    let context = ctx_2d
end)

module Font = Graphv_font.Fontstash.Make(FontJS)

include Graphv_core.Make(Gles2)(Font)

