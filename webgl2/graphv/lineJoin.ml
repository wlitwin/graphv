type t = Miter | Bevel | Round

let equal a b =
    match a, b with
    | Miter, Miter
    | Bevel, Bevel
    | Round, Round -> true
    | _ -> false
