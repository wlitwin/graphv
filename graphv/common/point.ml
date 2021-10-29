type flags = int array
type data = float array

type t = {
  mutable flags : flags;
  mutable data : data;
  mutable size : int;
}

let count = 7

let create () = 
  let initial_size = 1000 in
  {
    size = 0;
    data = Array.make (initial_size*count) 0.;
    flags = Array.make initial_size 0;
  }

let set : data -> int -> float -> unit = Array.unsafe_set
let get : data -> int -> float  = Array.unsafe_get
let length : data -> int = Array.length

let [@inline always] set_xy (t : t) (idx : int) x y =
  set t.data (idx*count) x;
  set t.data (idx*count+1) y;
;;

let resize (t : t) =
    let len = length t.data / count in
    let new_data : data = Array.init ((len*3/2)*count) (fun idx ->
        if idx < len*count then Array.unsafe_get t.data idx else 0.
    ) in
    let new_flags = Array.init (len*3/2) (fun idx ->
        if idx < len then Array.unsafe_get t.flags idx else 0
    ) in
    t.data <- new_data;
    t.flags <- new_flags;
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

let add (t : t) x y flags =
  if t.size >= Array.length t.flags then (
      resize t
  );
  let idx = t.size in
  t.size <- t.size + 1;
  Array.unsafe_set t.flags idx flags;
  let idx = idx*count in
  set t.data (idx) x;
  set t.data (idx+1) y;
  set t.data (idx+2) 0.;
  set t.data (idx+3) 0.;
  set t.data (idx+4) 0.;
  set t.data (idx+5) 0.;
  set t.data (idx+6) 0.;
;;

let [@inline always] get_last_x (t : t) =
  get t.data ((t.size-1)*count)

let [@inline always] get_last_y (t : t) =
  get t.data ((t.size-1)*count+1)

let normalize_pt (t : t) idx =
    let idx = idx*count in
    let t = t.data in
    let dx = get t (idx+2) in
    let dy = get t (idx+3) in
    let open Graphv_core_lib.FloatOps in
    let d = Float.sqrt (dx*dx + dy*dy) in
    set t (idx+.4) d;
    if d > 1e-6 then (
        let id = 1. / d in
        set t (idx+.2) (dx*id);
        set t (idx+.3) (dy*id);
    );
;;

let [@inline always] triarea2 ax ay bx by cx cy =
    let open Graphv_core_lib.FloatOps in
    let abx = bx - ax in
    let aby = by - ay in
    let acx = cx - ax in
    let acy = cy - ay in
    acx*aby - abx*acy
;;

let [@inline always] triarea2 t a b c =
  let t = t.data in
  let a = a*count in
  let ax = get t (a) in
  let ay = get t (a+1) in
  let b = b*count in
  let bx = get t (b) in
  let by = get t (b+1) in
  let c = c*count in
  let cx = get t (c) in
  let cy = get t(c+1) in
  triarea2 ax ay bx by cx cy
;;

let [@inline always] last (t : t) = t.size-1

let [@inline always] add_last_flag (t : t) flags =
  let idx = (t.size-1)*count in
  let last = Array.unsafe_get t.flags idx in
  Array.unsafe_set t.flags idx (last lor flags)
;;

let last_equals (t : t) x y tol =
  let idx = (t.size-1)*count in
  let lx = get t.data idx in
  let ly = get t.data (idx+1) in
  equals lx ly x y tol
;;

let equal_index (t : t) idx1 idx2 tol =
  let idx1 = idx1*count in
  let idx2 = idx2*count in
  let data = t.data in
  let x1 = get data idx1 in
  let x2 = get data idx2 in
  let y1 = get data (idx1+1) in
  let y2 = get data (idx2+1) in
  equals x1 y1 x2 y2 tol
;;

let [@inline always] assign_dx_dy (t : t) p0 p1 =
  let data = t.data in
  let p0 = p0*count in
  let p1 = p1*count in
  let x0 = get data p0 in
  let x1 = get data p1 in
  let y0 = get data (p0+1) in
  let y1 = get data (p1+1) in
  set data (p0+2) (x1 -. x0);
  set data (p0+3) (y1 -. y0);
  (*
          !p0.dx <- p1.x - !p0.x;
          !p0.dy <- p1.y - !p0.y;
     *)
;;

let [@inline always] get_x (t : data) idx =
  get t (idx*count)
let [@inline always] get_y (t : data) idx =
  get t (idx*count+1)
let [@inline always] get_dx (t : data) idx =
  get t (idx*count+2)
let [@inline always] get_dy (t : data) idx =
  get t (idx*count+3)

let [@inline always] len (t : data) idx =
  get t (idx*count+4)

let [@inline always] set_len (t : data) idx v =
  set t (idx*count+4) v

let [@inline always] set_dmx (t : data) idx v =
  set t (idx*count+5) v
let [@inline always] set_dmy (t : data) idx v =
  set t (idx*count+6) v

let [@inline always] get_dmx (t : data) idx =
  get t (idx*count+5)
let [@inline always] get_dmy (t : data) idx =
  get t (idx*count+6)

let [@inline always] set_flag (t : flags) idx v =
  Array.unsafe_set t idx v

let [@inline always] has_flag (t : flags) idx flag =
  Array.unsafe_get t idx land flag > 0

let [@inline always] add_flag (t : flags) idx flag =
  let idx = idx in
  Array.unsafe_set t idx (Array.unsafe_get t idx lor flag)

let [@inline always] get_xy (t : t) idx =
  let data = t.data in
  let idx = idx*count in
  let x = get data idx in
  let y = get data (idx+1) in
  x, y
;;

let [@inline always] length (t : t) : int = t.size

let [@inline always] clear (t : t) =
  t.size <- 0
