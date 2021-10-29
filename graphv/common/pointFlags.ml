type t = int
let no_flags = 0
let corner = 1
let left = 2
let bevel = 4
let inner_bevel = 8

let has t ~flag =
    t land flag > 0

let add t ~flag =
    t lor flag

let remove t ~flag =
    t land (lnot flag)
