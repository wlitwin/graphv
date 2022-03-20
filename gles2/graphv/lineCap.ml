type t = Butt | Round | Square | Default

let equal a b =
    match a, b with
    | Butt, Butt
    | Round, Round
    | Square, Square
    | Default, Default -> true
    | _ -> false
