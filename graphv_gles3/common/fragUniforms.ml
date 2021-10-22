module Color = Graphv_core_lib.Color

type t = Dyn.t

let count = 44

let byte_size = count*4

let set t i i2 v = 
  Dyn.set t (i + i2) v

let get t i i2 = 
  Dyn.get t (i + i2)

let create () =
  Dyn.create (count*4*100)

let set_scissor_mat t z a b c d e f g h i j k l =
    set t z 0 a;
    set t z 1 b;
    set t z 2 c;
    set t z 3 d;
    set t z 4 e;
    set t z 5 f;
    set t z 6 g;
    set t z 7 h;
    set t z 8 i;
    set t z 9 j;
    set t z 10 k;
    set t z 11 l;
;;

let set_paint_mat t z a b c d e f g h i j k l =
    set t z 12 a;
    set t z 13 b;
    set t z 14 c;
    set t z 15 d;
    set t z 16 e;
    set t z 17 f;
    set t z 18 g;
    set t z 19 h;
    set t z 20 i;
    set t z 21 j;
    set t z 22 k;
    set t z 23 l;
;;

let set_inner_color t i (color : Color.t) =
    set t i 24 color.r;
    set t i 25 color.g;
    set t i 26 color.b;
    set t i 27 color.a;
;;

let set_outer_color t i (color : Color.t) =
    set t i 28 color.r;
    set t i 29 color.g;
    set t i 30 color.b;
    set t i 31 color.a;
;;
    
let set_scissor_ext t i a b =
    set t i 32 a;
    set t i 33 b;
;;

let set_scissor_scale t i a b =
    set t i 34 a;
    set t i 35 b;
;;

let set_extent t i a b =
    set t i 36 a;
    set t i 37 b;
;;

let get_extent1 t i =
    get t i 37

let reset t z =
  for i=0 to (count*4) do
    Dyn.set t (z + i) 0.
  done
;;

let set_radius t i r      = set t i 38 r
let set_feather t i f     = set t i 39 f
let set_stroke_mult t i s = set t i 40 s
let set_stroke_thr t i s  = set t i 41 s
let set_tex_type t i typ  = set t i 42 typ
let set_type t i typ      = set t i 43 typ

let as_array t = Dyn.unsafe_array t

let make_slot t num align =
  Dyn.add_range t (num * align)
