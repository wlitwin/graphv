include Graphv_webgl

open Js_of_ocaml

let canvas : Dom_html.canvasElement Js.t = 
    Js.Unsafe.coerce (Dom_html.getElementById_exn "canvas")
;;

let ctx_webgl =
    let attrs = WebGL.defaultContextAttributes in
    attrs##.antialias := Js._false;
    attrs##.stencil := Js._true;
    WebGL.getContextWithAttributes canvas attrs
    |> Js.Opt.to_option
    |> function
       | None -> failwith "Expected context"
       | Some ctx -> ctx
;;

let _ =
    Js.Unsafe.global##.wctx := ctx_webgl

