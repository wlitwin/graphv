open Graphv_core_lib

type t = {
    mutable composite_operation : CompositeOperationState.t;
    mutable shape_anti_alias : bool;
    mutable fill : Paint.t;
    mutable stroke : Paint.t;
    mutable stroke_width : float;
    mutable miter_limit : float;
    mutable line_join : LineJoin.t;
    mutable line_cap : LineCap.t;
    mutable alpha : float;
    xform : Matrix.t;
    mutable scissor : Scissor.t;
    mutable font_size : float;
    mutable letter_spacing : float;
    mutable line_height : float;
    mutable font_blur : float;
    mutable text_align : Align.t;
    mutable font_id : int;
}

let create () = {
    composite_operation = 
        CompositeOperationState.of_composite_operation CompositeOperation.Source_over;
    shape_anti_alias = false;
    fill = Paint.create();
    stroke = Paint.create();
    stroke_width = 1.;
    miter_limit = 0.;
    line_join = LineJoin.Miter;
    line_cap = LineCap.Butt;
    alpha = 1.;
    xform = Matrix.create();
    scissor = Scissor.create();
    font_size = 16.;
    letter_spacing = 0.;
    line_height = 1.;
    font_blur = 0.;
    text_align = Align.left;
    font_id = 0;
}

let copy (t : t) : t = {
    t with
    xform = Matrix.copy t.xform;
    scissor = Scissor.copy t.scissor;
    fill = Paint.copy t.fill;
    stroke = Paint.copy t.stroke;
}
;;

let reset t =
    t.composite_operation <-
        CompositeOperationState.of_composite_operation CompositeOperation.Source_over;
    t.shape_anti_alias <- false;
    t.fill <- Paint.create();
    t.stroke <- Paint.create();
    t.stroke_width <- 1.;
    t.miter_limit <- 0.;
    t.line_join <- LineJoin.Miter;
    t.line_cap <- LineCap.Butt;
    t.alpha <- 1.;
    Matrix.identity t.xform;
    t.scissor <- Scissor.create();
    t.font_size <- 16.;
    t.letter_spacing <- 0.;
    t.line_height <- 1.;
    t.font_blur <- 0.;
    t.text_align <- Align.left;
    t.font_id <- 0;
