type t = {
    xform : Matrix.t;
    extent_0 : float;
    extent_1 : float;
}

let create() = {
    xform = Matrix.create();
    extent_0 = ~-.1.;
    extent_1 = ~-.1.;
}

let copy t = {
    t with xform = Matrix.copy t.xform
}
