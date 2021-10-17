type t = float -> float

let linear (x : float) = x

let in_sine (x : float) =
    1. -. Float.cos ((x *. Float.pi) /. 2.)

let out_sine (x : float) =
    Float.sin ((x *. Float.pi) /. 2.)

let in_out_sine (x : float) =
    ~-.(Float.cos (Float.pi *. x) -. 1.) /. 2.

let in_quad (x : float) =
    x*.x

let out_quad (x : float) =
    1. -. (1. -. x) *. (1. -. x)

let in_out_quad (x : float) =
    if x < 0.5 then (2. *. x *. x)
    else (1. -. (Float.pow (~-.2. *. x +. 2.) 2.) /. 2.)

let in_cubic (x : float) =
    x *. x *. x

let out_cubic (x : float) =
    1. -. (Float.pow (1. -. x) 3.)

let in_out_cubic (x : float) =
    if x < 0.5 then (4. *. x *. x *. x)
    else (1. -. (Float.pow (~-.2. *. x +. 2.) 3.) /. 2.)

let in_quart (x : float) =
     x *. x *. x *. x

let out_quart (x : float) =
    1. -. (Float.pow (1. -. x) 4.)

let in_out_quart (x : float) =
    if x < 0.5 then (8. *. x *. x *. x *. x) 
    else (1. -. (Float.pow (~-.2. *. x +. 2.) 4.) /. 2.)

let in_quint (x : float) =
    x *. x *. x *. x *. x

let out_quint (x : float) =
    1. -. (Float.pow (1. -. x) 5.)

let in_out_quint (x : float) =
    if x < 0.5 then (16. *. x *. x *. x *. x *. x) 
    else (1. -. (Float.pow (~-.2. *. x +. 2.) 5.) /. 2.)

let in_expo (x : float) =
    if x <= 0. then 0. else (Float.pow 2. (10. *. x -. 10.))

let out_expo (x : float) =
    if x >= 1. then 1. else (1. -. (Float.pow 2. (~-.10. *. x)))

let in_out_expo (x : float) =
    if x <= 0. then 0.
    else if x >= 1. then 1.
    else if x < 0.5 then (
        (Float.pow 2. (20. *. x -. 10.)) /. 2.
    ) else (
        2. -. (Float.pow 2. (~-.20. *. x +. 10.)) /. 2.
    )

let in_circ (x : float) =
    1. -. (Float.sqrt (1. -. (Float.pow x 2.)))

let out_circ (x : float) =
    Float.sqrt (1. -. (Float.pow (x -. 1.) 2.))

let in_out_circ (x : float) =
    if x < 0.5 then (
        1. -. (Float.sqrt (1. -. (Float.pow (2. *. x) 2.))) /. 2.
    ) else (
        Float.sqrt (1. -. (Float.pow (~-.2. *. x +. 2.) 2.) +. 1.) /. 2.
    )

let in_back (x : float) =
    let c1 = 1.70158 in
    let c3 = c1 +. 1. in
    c3 *. x *. x *. x -. c1 *. x *. x

let out_back (x : float) =
    let c1 = 1.70158 in
    let c3 = c1 +. 1. in
    1. +. c3 *. (Float.pow (x -. 1.) 3.) +. c1 *. (Float.pow (x -. 1.) 2.)

let in_out_back (x : float) =
    let c1 = 1.70158 in
    let c2 = c1 *. 1.525 in

    if x < 0.5 then (
        ((Float.pow (2. *. x) 2.) *. ((c2 +. 1.) *. 2. *. x -. c2)) /. 2.
    ) else (
        ((Float.pow (2. *. x -. 2.) 2.) *. ((c2 +. 1.) *. (x *. 2. -. 2.) +. c2) +. 2.) /. 2.
    )

let in_elastic (x : float) =
    let c4 = (2. *. Float.pi) /. 3. in
    if x <= 0. then 0.
    else if x >= 1. then 1.
    else (
        ~-.(Float.pow 2. (10. *. x -. 10.)) *. (Float.sin ((x *. 10. -. 10.75) *. c4))
    )

let out_elastic (x : float) =
    let c4 = (2. *. Float.pi) /. 3. in
    if x <= 0. then 0.
    else if x >= 1. then 1.
    else (
      (Float.pow 2. (~-.10. *. x)) *. (Float.sin ((x *. 10. -. 0.75) *. c4)) +. 1.
    )

let in_out_elastic (x : float) =
    let c5 = (2. *. Float.pi) /. 4.5 in
    if x <= 0. then 0.
    else if x >= 1. then 1.
    else if x < 0.5 then (
        ~-.(Float.pow 2. (20. *. x -. 10.)) *. (Float.sin ((20. *. x -. 11.125) *. c5)) /. 2.
    ) else (
        (Float.pow 2. (~-.20. *. x +. 10.)) *. (Float.sin ((20. *. x -. 11.125) *. c5)) /. 2. +. 1.
    )

let out_bounce (x : float) =
    let n1 = 7.5625 in
    let d1 = 2.75 in

    if x < 1. /. d1 then (n1 *. x *. x)
    else if x < 2. /. d1 then (
        let x = x -. (1.5 /. d1) in
        n1 *. x *. x +. 0.75
    ) else if x < 2.5 /. d1 then (
        let x = x -. (2.25 /. d1) in
        n1 *. x *. x +. 0.9375
    ) else (
        let x = x -. (2.625 /. d1) in
        n1 *. x *. x +. 0.984375
    )

let in_bounce (x : float) =
    1. -. (out_bounce (1. -. x))

let in_out_bounce (x : float) =
    if x < 0.5 then (
      1. -. (out_bounce (1. -. 2. *. x)) /. 2.
    ) else (
      1. +. (out_bounce (2. *. x -. 1.)) /. 2.
    )

