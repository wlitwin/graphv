type t = {
    r : float;
    g : float;
    b : float;
    a : float;
}

let premultiply t = {
    t with
    r = t.r *. t.a;
    g = t.g *. t.a;
    b = t.b *. t.a;
}

let rgbaf ~r ~g ~b ~a = {
    r; g; b; a;
}

let rgbf ~r ~g ~b = 
    rgbaf ~r ~g ~b ~a:1.

let rgba ~r ~g ~b ~a = {
    r = float r /. 255.;
    g = float g /. 255.;
    b = float b /. 255.;
    a = float a /. 255.;
}

let rgb ~r ~g ~b = 
    rgba ~r ~g ~b ~a:255

let white = rgbf ~r:1. ~g:1. ~b:1.
let black = rgbf ~r:0. ~g:0. ~b:0.
let transparent = rgbaf ~r:0. ~g:0. ~b:0. ~a:0.

let lerp a b t =
    if t <= 0.5 then a+.(b-.a)*.t
    else b-.(b-.a)*.(1.-.t)

let lerp c1 c2 ~a = {
    r = lerp c1.r c2.r a;
    g = lerp c1.g c2.g a;
    b = lerp c1.b c2.b a;
    a = lerp c1.a c2.a a;
}

let clamp (v : float) (min : float) (max : float) : float =
    if v < min then min
    else if v > max then max
    else v
;;

let transf c a = { c with a }

let trans c a = { c with a = float a /. 255. }

let hue h m1 m2 =
    let open FloatOps in
    let h =
        if h < 0. then h + 1.
        else if h > 1. then h - 1.
        else h
    in
    if h < 1./6. then 
        m1 + (m2 - m1) * h * 6.
    else if h < 3./6. then
        m2
    else if h < 4./6. then
        m1 + (m2 - m1) * (2./3. - h) * 6.
    else m1
;;

let hsla h s l a =
    let open FloatOps in
    let h = 
        let h = Float.rem h 1. in
        if h < 0. then h + 1. else h
    in
    let s = clamp s 0. 1. in
    let l = clamp l 0. 1. in
    let m2 = if l <= 0.5 then l * (1. + s) else l + s - l*s in
    let m1 = 2.*l - m2 in
    let r = clamp (hue (h + 1./3.) m1 m2) 0. 1. in
    let g = clamp (hue h m1 m2) 0. 1. in
    let b = clamp (hue (h - 1./3.) m1 m2) 0. 1. in
    let a = float a / 255. in
    { r; g; b; a }
;;

let hsl ~h ~s ~l = 
    hsla h s l 255

let hsla ~h ~s ~l ~a =
    hsla h s l a
