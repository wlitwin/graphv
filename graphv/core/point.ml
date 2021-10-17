type t = {
    mutable x : float;
    mutable y : float;
    mutable dx : float;
    mutable dy : float;
    mutable len  : float;
    mutable dmx : float;
    mutable dmy : float;
    mutable flags : PointFlags.t;
}

let create ~x ~y ~flags = {
    x; y; flags;
    dx = 0.;
    dy = 0.;
    len = 0.;
    dmx = 0.;
    dmy = 0.;
}

let empty () =
    create ~x:0. ~y:0. ~flags:PointFlags.no_flags

let reset t x y flags =
    t.x <- x;
    t.y <- y;
    t.flags <- flags;
    t.dx <- 0.;
    t.dy <- 0.;
    t.len <- 0.;
    t.dmx <- 0.;
    t.dmy <- 0.;
;;

let equals x1 y1 x2 y2 tol =
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in
    dx*.dx +. dy*.dy < tol*.tol
;;

let dist_segment x y px py qx qy =
    let pqx = qx -. px in
    let pqy = qy -. py in
    let dx = x -. px in
    let dy = y -. py in
    let d = pqx*.pqx +. pqy*.pqy in
    let t = pqx*.dx +. pqy*.dy in
    let t = if d > 0. then t /. d else t in
    let t = if t < 0. then 0. else if t > 1. then 1. else t in
    let dx = px +. t*.pqx -. x in 
    let dy = py +. t*.pqy -. y in
    dx*.dx +. dy*.dy
;;

let normalize x y =
    let open Graphv_core_lib.FloatOps in
    let d = Float.sqrt (x*x + y*y) in
    if d > 1e-6 then (
        let id = 1. / d in
        d, x*id, y*id
    ) else (
        d, x, y
    )
;;
