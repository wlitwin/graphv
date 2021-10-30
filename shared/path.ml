type t = {
    mutable first : int;
    mutable count : int;
    mutable closed : bool;
    mutable nbevel : int;
    mutable fill : VertexBuffer.Sub.t;
    mutable stroke : VertexBuffer.Sub.t;
    mutable winding : Graphv_core_lib.Winding.t;
    mutable convex : bool;
}

let empty_sub = VertexBuffer.Sub.create()

let create () = {
    first = 0;
    count = 0;
    closed = false;
    nbevel = 0;
    fill = empty_sub;
    stroke = empty_sub;
    winding = Graphv_core_lib.Winding.CCW;
    convex = true;
}

let reset (t : t) : unit =
    t.first <- 0;
    t.count <- 0;
    t.closed <- false;
    t.nbevel <- 0;
    t.fill <- empty_sub;
    t.stroke <- empty_sub;
    t.winding <- Graphv_core_lib.Winding.CCW;
    t.convex <- true;
;;

let copy (t : t) : t = {
    first = t.first;
    count = t.count;
    closed = t.closed;
    nbevel = t.nbevel;
    fill = t.fill;
    stroke = t.stroke;
    winding = t.winding;
    convex = t.convex;
}

