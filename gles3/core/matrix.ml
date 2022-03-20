type t = {
    mutable m0 : float;
    mutable m1 : float;
    mutable m2 : float;
    mutable m3 : float;
    mutable m4 : float;
    mutable m5 : float;
}

let create () = {
    m0 = 0.;
    m1 = 0.;
    m2 = 0.;
    m3 = 0.;
    m4 = 0.;
    m5 = 0.;
}

let zero t =
    t.m0 <- 0.;
    t.m1 <- 0.;
    t.m2 <- 0.;
    t.m3 <- 0.;
    t.m4 <- 0.;
    t.m5 <- 0.;
;;

let [@inline always] identity t =
    t.m0 <- 1.; t.m1 <- 0.;
    t.m2 <- 0.; t.m3 <- 1.;
    t.m4 <- 0.; t.m5 <- 0.;
;;

let [@inline always] translate dst ~x ~y =
    dst.m0 <- 1.; dst.m1 <- 0.;
    dst.m2 <- 0.; dst.m3 <- 1.;
    dst.m4 <- x;  dst.m5 <- y;
;;

let [@inline always] scale dst ~xs ~ys =
    dst.m0 <- xs; dst.m1 <- 0.;
    dst.m2 <- 0.; dst.m3 <- ys;
    dst.m4 <- 0.; dst.m5 <- 0.;
;;

let [@inline always] transform_point t xs ys =
    let open FloatOps in
    let x = xs*t.m0 + ys*t.m2 + t.m4 in
    let y = xs*t.m1 + ys*t.m3 + t.m5 in
    x, y
;;

let rotate dst ~angle =
    let cs = Float.cos angle
    and sn = Float.sin angle in
    dst.m0 <- cs; dst.m1 <- sn;
    dst.m2 <- ~-.sn; dst.m3 <- cs;
    dst.m4 <- 0.; dst.m5 <- 0.;
;;

let skew_x t ~angle =
    t.m0 <- 1.; t.m1 <- 0.;
    t.m2 <- Float.tan angle; t.m3 <- 1.;
    t.m4 <- 0.; t.m5 <- 0.;
;;

let skew_y t ~angle =
    t.m0 <- 1.; t.m1 <- Float.tan angle;
    t.m2 <- 0.; t.m3 <- 1.;
    t.m4 <- 0.; t.m5 <- 0.;
;;

let multiply ~dst ~src =
    let open FloatOps in
    let t0 = dst.m0*src.m0 + dst.m1*src.m2 in
    let t2 = dst.m2*src.m0 + dst.m3*src.m2 in
    let t4 = dst.m4*src.m0 + dst.m5*src.m2 + src.m4 in
    dst.m1 <- dst.m0*src.m1 + dst.m1*src.m3;
    dst.m3 <- dst.m2*src.m1 + dst.m3*src.m3;
    dst.m5 <- dst.m4*src.m1 + dst.m5*src.m3 + src.m5;

    dst.m0 <- t0;
    dst.m2 <- t2;
    dst.m4 <- t4;
;;

let [@inline always] copy (t : t) : t = {
    t with m0 = t.m0
}

let [@inline always] premultiply ~dst ~src =
    let open FloatOps in
    let t0 = src.m0*dst.m0 + src.m1*dst.m2 in
    let t2 = src.m2*dst.m0 + src.m3*dst.m2 in
    let t4 = src.m4*dst.m0 + src.m5*dst.m2 + dst.m4 in
    let t1 = src.m0*dst.m1 + src.m1*dst.m3 in
    let t3 = src.m2*dst.m1 + src.m3*dst.m3 in
    let t5 = src.m4*dst.m1 + src.m5*dst.m3 + dst.m5 in

    dst.m0 <- t0;
    dst.m1 <- t1;
    dst.m2 <- t2;
    dst.m3 <- t3;
    dst.m4 <- t4;
    dst.m5 <- t5;
;;

let inverse ~dst ~src =
    let open FloatOps in
    let det = src.m0*src.m3 - src.m2*src.m1 in
    if det > ~-.1e-6 && det < 1e-6 then (
        identity dst
    ) else (
        let invdet = 1.0 / det in
        let t0 = src.m0 in
        let t1 = src.m1 in
        let t2 = src.m2 in
        let t3 = src.m3 in
        let t4 = src.m4 in
        let t5 = src.m5 in

        dst.m0 <- t3 * invdet; 
        dst.m2 <- (~-.t2) * invdet;
        dst.m4 <- (t2 * t5 - t3 * t4) * invdet;
        dst.m1 <- (~-.t1) * invdet;
        dst.m3 <- t0 * invdet;
        dst.m5 <- (t1 * t4 - t0 * t5) * invdet;
    )
;;

let to_3x4 (t : t) = [|
    t.m0; t.m1; 0.; 0.;
    t.m2; t.m3; 0.; 0.;
    t.m4; t.m5; 1.; 0.;
|]

let get_average_scale t =
    let open FloatOps in
    let sx = Float.sqrt ( (t.m0*t.m0) + (t.m2*t.m2) ) in 
    let sy = Float.sqrt ( (t.m1*t.m1) + (t.m3*t.m3) ) in
    (sx + sy) * 0.5
;;

let [@inline always] is_flipped t =
    let open FloatOps in
    let det = t.m0*t.m3 - t.m2*t.m1 in
    det < 0.
;;

let print t =
    Printf.printf "%.2f %.2f\n" t.m0 t.m1;
    Printf.printf "%.2f %.2f\n" t.m2 t.m3;
    Printf.printf "%.2f %.2f\n" t.m4 t.m5;
;;
