type t = {
    (*xform : Matrix.t;*)
    mutable m0 : float;
    mutable m1 : float;
    mutable m2 : float;
    mutable m3 : float;
    mutable m4 : float;
    mutable m5 : float;
    mutable extent_x : float;
    mutable extent_y : float;
    mutable radius : float;
    mutable feather : float;
    mutable inner_color_r : float;
    mutable inner_color_g : float;
    mutable inner_color_b : float;
    mutable inner_color_a : float;
    mutable outer_color_r : float;
    mutable outer_color_g : float;
    mutable outer_color_b : float;
    mutable outer_color_a : float;
    mutable image : float;
}

let create () = {
    m0 = 1.;
    m1 = 0.;
    m2 = 0.;
    m3 = 1.;
    m4 = 0.;
    m5 = 0.;
    extent_x = 0.;
    extent_y = 0.;
    radius = 0.;
    feather = 0.;
    inner_color_r = 1.;
    inner_color_g = 1.;
    inner_color_b = 1.;
    inner_color_a = 1.;
    outer_color_r = 0.;
    outer_color_g = 0.;
    outer_color_b = 0.;
    outer_color_a = 1.;
    image = 0.;
}

let copy t = {
    t with m0 = t.m0
}

let modify_alpha (t : t) (alpha : float) =
    t.inner_color_a <- t.inner_color_a *. alpha;
    t.outer_color_a <- t.outer_color_a *. alpha;
;;

(*
let set_inner_color (t : t) (color : Color.t) (alpha : float) =
    t.inner_color_r <- color.r;
    t.inner_color_g <- color.g;
    t.inner_color_b <- color.b;
    t.inner_color_a <- color.a *. alpha;
;;

let set_outer_color (t : t) (color : Color.t) (alpha : float) =
    t.outer_color_r <- color.r;
    t.outer_color_g <- color.g;
    t.outer_color_b <- color.b;
    t.outer_color_a <- color.a *. alpha;
;;
*)

let reset_xform (t : t) =
    t.m0 <- 1.;
    t.m1 <- 0.;
    t.m2 <- 0.;
    t.m3 <- 1.;
    t.m4 <- 0.;
    t.m5 <- 0.;
;;

let rotate (t : t) (angle : float) =
    let cs = Float.cos angle
    and sn = Float.sin angle in
    t.m0 <- cs; t.m1 <- sn;
    t.m2 <- ~-.sn; t.m3 <- cs;
    t.m4 <- 0.; t.m5 <- 0.;
;;

let reset_colors_with_alpha (t : t) (alpha : float) =
    t.inner_color_r <- 1.;
    t.inner_color_g <- 1.;
    t.inner_color_b <- 1.;
    t.inner_color_a <- alpha;
    t.outer_color_r <- 1.;
    t.outer_color_g <- 1.;
    t.outer_color_b <- 1.;
    t.outer_color_a <- alpha;
;;

let change_color_keep_extent (t : t) (color : Color.t) : unit =
    t.m0 <- 1.;
    t.m1 <- 0.;
    t.m2 <- 0.;
    t.m3 <- 1.;
    t.m4 <- 0.;
    t.m5 <- 0.;
    t.radius <- 0.;
    t.feather <- 1.;
    t.inner_color_r <- color.r;
    t.inner_color_g <- color.g;
    t.inner_color_b <- color.b;
    t.inner_color_a <- color.a;

    t.outer_color_r <- color.r;
    t.outer_color_g <- color.g;
    t.outer_color_b <- color.b;
    t.outer_color_a <- color.a;
;;

let set_only_inner_and_outer (t : t) (inner : Color.t) (outer : Color.t) =
    t.inner_color_r <- inner.r;
    t.inner_color_g <- inner.g;
    t.inner_color_b <- inner.b;
    t.inner_color_a <- inner.a;
    t.outer_color_r <- outer.r;
    t.outer_color_g <- outer.g;
    t.outer_color_b <- outer.b;
    t.outer_color_a <- outer.a;
;;

let set_color (t : t) (color : Color.t) =
    t.m0 <- 1.;
    t.m1 <- 0.;
    t.m2 <- 0.;
    t.m3 <- 1.;
    t.m4 <- 0.;
    t.m5 <- 0.;
    t.extent_x <- 0.;
    t.extent_y <- 0.;
    t.radius <- 0.;
    t.feather <- 1.;
    t.inner_color_r <- color.r;
    t.inner_color_g <- color.g;
    t.inner_color_b <- color.b;
    t.inner_color_a <- color.a;
    t.outer_color_r <- color.r;
    t.outer_color_g <- color.g;
    t.outer_color_b <- color.b;
    t.outer_color_a <- color.a;
;;

let multiply (t : t) (src : Matrix.t) =
    let open FloatOps in
    let t0 = t.m0*src.m0 + t.m1*src.m2 in
    let t2 = t.m2*src.m0 + t.m3*src.m2 in
    let t4 = t.m4*src.m0 + t.m5*src.m2 + src.m4 in
    t.m1 <- t.m0*src.m1 + t.m1*src.m3;
    t.m3 <- t.m2*src.m1 + t.m3*src.m3;
    t.m5 <- t.m4*src.m1 + t.m5*src.m3 + src.m5;

    t.m0 <- t0;
    t.m2 <- t2;
    t.m4 <- t4;
;;

let extract_xform (t : t) =
    Matrix.{
        m0 = t.m0;
        m1 = t.m1;
        m2 = t.m2;
        m3 = t.m3;
        m4 = t.m4;
        m5 = t.m5;
    }
;;
