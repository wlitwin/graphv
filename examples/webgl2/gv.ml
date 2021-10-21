include Graphv_webgl2

open Js_of_ocaml

let canvas : Dom_html.canvasElement Js.t = 
    Js.Unsafe.coerce (Dom_html.getElementById_exn "canvas")
;;

let ctx_webgl : WebGL.renderingContext Js.t =
    let open Js.Unsafe in
    let options = (Js.Unsafe.coerce (object%js
            val antialias = Js._false
            val stencil = Js._true
        end))
    in
    let ctx : WebGL.renderingContext Js.t = 
      meth_call canvas "getContext" [|inject (Js.string "webgl2"); inject options|]
    in
    ctx
;;

let _ =
    Js.Unsafe.global##.wctx := ctx_webgl

