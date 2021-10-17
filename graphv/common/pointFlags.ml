type t = float
let no_flags = 0.
let corner = 1.
let left = 2.
let bevel = 4.
let inner_bevel = 8.

let has t ~flag =
    ((int_of_float t) land (int_of_float flag)) > 0

let add t ~flag =
    (int_of_float t) lor (int_of_float flag) |> float

let remove t ~flag =
    (int_of_float t) land (lnot (int_of_float flag)) |> float
