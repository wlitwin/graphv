type t = CCW | CW
let ccw = CCW
let cw = CW

let hole = CW
let solid = CCW

let equal a b =
    match a, b with
    | CCW, CCW
    | CW, CW -> true
    | _ -> false
