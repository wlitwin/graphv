let safe_binop op a b =
    let c = op a b in
    if Float.is_nan c then (
        failwith "NAN!"
    );
    c

let ( -. ) = safe_binop (-.)
let ( +. ) = safe_binop (+.)
let ( *. ) = safe_binop ( *.)
let ( /. ) = safe_binop (/.)
