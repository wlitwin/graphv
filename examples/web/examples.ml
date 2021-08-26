open NVG

(** Draw a donut. Use opposite windings to create a hole. *)
let donut vg =
    Path.begin_ vg;
    Path.arc vg ~cx:150. ~cy:150. ~r:100. ~a0:0. ~a1:(Float.pi*.2.) ~dir:Winding.CW;
    Path.arc vg ~cx:150. ~cy:150. ~r:50. ~a0:0. ~a1:(Float.pi*.2.) ~dir:Winding.CCW;
    set_fill_color vg ~color:Color.white;
    fill vg;
;;

(** Bezier curves. *)
let splines vg =
    Path.begin_ vg;
    set_stroke_width vg ~width:2.;
    Path.move_to vg ~x:200. ~y:200.;
    Path.bezier_to vg ~c1x:100. ~c1y:100. ~c2x:200. ~c2y:50. ~x:300. ~y:100.;
    set_stroke_color vg ~color:Color.white;
    stroke vg;
;;
