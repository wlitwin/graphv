open Graphv_core_lib

module Make(Buffer : sig 
    type buffer
    val create : int -> buffer
    val set : buffer -> int -> float -> unit
    val get : buffer -> int -> float
end) = struct

type t = Buffer.buffer

let count = 11

let set = Buffer.set
let get = Buffer.get

let create () =
  Buffer.create (count*4)

let set_scissor_mat t a b c d e f g h i j k l =
    set t 0 a;
    set t 1 b;
    set t 2 c;
    set t 3 d;
    set t 4 e;
    set t 5 f;
    set t 6 g;
    set t 7 h;
    set t 8 i;
    set t 9 j;
    set t 10 k;
    set t 11 l;
;;

let set_paint_mat t a b c d e f g h i j k l =
    set t 12 a;
    set t 13 b;
    set t 14 c;
    set t 15 d;
    set t 16 e;
    set t 17 f;
    set t 18 g;
    set t 19 h;
    set t 20 i;
    set t 21 j;
    set t 22 k;
    set t 23 l;
;;

let set_inner_color t (color : Color.t) =
    set t 24 color.r;
    set t 25 color.g;
    set t 26 color.b;
    set t 27 color.a;
;;

let set_outer_color t (color : Color.t) =
    set t 28 color.r;
    set t 29 color.g;
    set t 30 color.b;
    set t 31 color.a;
;;
    
let set_scissor_ext t a b =
    set t 32 a;
    set t 33 b;
;;

let set_scissor_scale t a b =
    set t 34 a;
    set t 35 b;
;;

let set_extent t a b =
    set t 36 a;
    set t 37 b;
;;

let get_extent1 t =
    get t 37

let set_radius t r      = set t 38 r
let set_feather t f     = set t 39 f
let set_stroke_mult t s = set t 40 s
let set_stroke_thr t s  = set t 41 s
let set_tex_type t typ  = set t 42 typ
let set_type t typ      = set t 43 typ

let as_array t = t
end
