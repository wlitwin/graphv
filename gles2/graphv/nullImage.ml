type image = unit
type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
let create_mem ~data:_ = failwith "Unimplemented"
let create_rgba ~data:_ ~width:_ ~height:_ = failwith "Unimplemented"
let data _img = failwith "Unimplemented"
let size _img = failwith "Unimplemented"
