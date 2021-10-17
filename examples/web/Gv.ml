include Graphv_webgl

open Js_of_ocaml

let canvas : Dom_html.canvasElement Js.t = 
    Js.Unsafe.coerce (Dom_html.getElementById_exn "canvas")
;;

let ctx_webgl : WebGL.renderingContext Js.t =
    WebGL.getContextWithAttributes canvas
    (Js.Unsafe.coerce (object%js
        val antialias = Js._false
        val stencil = Js._true
    end))
    |> Js.Opt.to_option
    |> function
       | None -> failwith "Expected context"
       | Some ctx -> ctx
;;

let _ =
    Js.Unsafe.global##.wctx := ctx_webgl

