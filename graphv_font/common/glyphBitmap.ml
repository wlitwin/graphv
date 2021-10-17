include Graphv_core_lib.Flags.Base
let optional = 1
let required = 2

type pattern = Optional
             | Required

let to_pattern = function
    | 1 -> Optional
    | 2 -> Required
    | _ -> failwith "impossible"
