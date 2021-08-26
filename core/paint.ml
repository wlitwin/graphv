type t = {
    xform : Matrix.t;
    mutable extent : float * float;
    mutable radius : float;
    mutable feather : float;
    mutable inner_color : Color.t;
    mutable outer_color : Color.t;
    mutable image : int;
}

let create () = {
    xform = Matrix.create();
    extent = 0., 0.;
    radius = 0.;
    feather = 0.;
    inner_color = Color.white;
    outer_color = Color.black;
    image = 0;
}

let copy t = {
    t with xform = Matrix.copy t.xform
}

let set_color (t : t) (color : Color.t) =
    Matrix.identity t.xform;
    t.extent <- 0., 0.;
    t.radius <- 0.;
    t.feather <- 1.;
    t.inner_color <- color;
    t.outer_color <- color;
;;
