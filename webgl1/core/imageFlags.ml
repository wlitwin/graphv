type t = int

let no_flags = 0
let generate_mipmaps = 1 lsl 0
let repeat_x = 1 lsl 1
let repeat_y = 1 lsl 2
let flip_y = 1 lsl 3
let premultiplied = 1 lsl 4
let nearest = 1 lsl 5
let (lor) = (lor)
let remove t ~flag =
    t land (lnot flag)
let has t ~flag =
    t land flag > 0
