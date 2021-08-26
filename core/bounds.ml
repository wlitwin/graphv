type t = {
    xmin : float; (*0*)
    ymin : float; (*1*)
    xmax : float; (*2*)
    ymax : float; (*3*)
}

let scale t s = {
    xmin = t.xmin *. s;
    ymin = t.ymin *. s;
    xmax = t.xmax *. s;
    ymax = t.ymax *. s;
}

let empty = {
    xmin = 0.;
    ymin = 0.;
    xmax = 0.;
    ymax = 0.;
}
