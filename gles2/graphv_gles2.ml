include Graphv_core

let create ~flags () =
    let gles = Gles.create ~flags () |> opt_exn in
    let font = Fontstash.create () in
    create ~flags gles font

