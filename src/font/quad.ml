type t = {
    mutable x0 : float;
    mutable y0 : float;
    mutable s0 : float;
    mutable t0 : float;
    mutable x1 : float;
    mutable y1 : float;
    mutable s1 : float;
    mutable t1 : float;
}

let empty () = {
    x0 = 0.;
    y0 = 0.;
    s0 = 0.;
    t0 = 0.;
    x1 = 0.;
    y1 = 0.;
    s1 = 0.;
    t1 = 0.;
}

let reset t =
    t.x0 <- 0.;
    t.y0 <- 0.;
    t.s0 <- 0.;
    t.t0 <- 0.;
    t.x1 <- 0.;
    t.y1 <- 0.;
    t.s1 <- 0.;
    t.t1 <- 0.;
