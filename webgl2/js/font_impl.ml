open Js_of_ocaml

module Buffer = Buffer.UByte

let some_exn v = 
    v
    |> Js.Optdef.to_option
    |> function
        | None -> failwith "index out of bounds"
        | Some v -> v
;;

type t = Dom_html.canvasRenderingContext2D Js.t

type font = {
    name : string;
    context : t;
}

type glyph = {
    str : Js.js_string Js.t;
    codepoint : int;
}

let invalid_glyph = {
    str = Js.string "";
    codepoint = ~-1;
}

module VMetrics = struct
    type t = {
        ascent : int;
        descent : int;
        line_gap : int;
    }
    let ascent t = t.ascent
    let descent t = t.descent
    let line_gap t = t.line_gap
end

module HMetrics = struct
    type t = {
        width : int;
        bearing : int;
    }
    let advance_width t = t.width
    let left_side_bearing t = t.bearing
end

module Box = struct
    type t = {
        x0 : int;
        y0 : int;
        x1 : int;
        y1 : int;
    }
    let x0 t = t.x0 
    let y0 t = t.y0
    let x1 t = t.x1
    let y1 t = t.y1
    let create x0 y0 x1 y1 = {
        x0; y0; x1; y1;
    }
end

let g_scale = 2146.

let vmetrics _font =
    VMetrics.{ 
      ascent = 2146; 
      descent = -555;
      line_gap = 0;
    }
;;

let hmetrics font (glyph : glyph) = 
    font.context##.font := Js.string ("12px " ^ font.name);
    let m = font.context##measureText glyph.str in
    HMetrics.{ width = Js.to_float m##.width *. 175. |> int_of_float;
               bearing = 0 }
;;

let get_glyph_bitmap_box font (glyph : glyph) ~scale = 
    let height = Js.number_of_float (scale *. g_scale) in
    let s = height##toString in
    font.context##.font := s##concat_2 (Js.string "px ") (Js.string font.name);
    let width = Js.to_float (font.context##measureText glyph.str)##.width in
    let width = int_of_float (width*.1.2) in
    (* pixel units based on font height ? *)
    let y = ~-.0.80 *. (scale *. g_scale) |> int_of_float in
    Box.create 1 y width (scale *. g_scale *. 0.22 |> int_of_float)
;;

(* returns a value that is scaled 0-g_scale*)
let kern_advance _font _glyph1 _glyph2 = 0

let create_font _data = 
    failwith "Unimplemented"

let load_font (t : t) name = Some {
    name;
    context = t;
}

let is_invalid_glyph (g : glyph) = (g.codepoint = ~-1)

let make_glyph_bitmap font (data : Buffer.t) ~width ~height:_ ~scale (box : Box.t) (glyph : glyph) =
    let font_height = Js.number_of_float (scale *. g_scale) in
    let s = font_height##toString in
    font.context##.font := s##concat_2 (Js.string "px ") (Js.string font.name);

    font.context##.textBaseline := (Js.string "top");
    font.context##.textAlign := (Js.string "left");

    let w = (box.x1 - box.x0) in
    let h = (box.y1 - box.y0) in

    font.context##clearRect
      (Js.float 0.) (Js.float 0.) (Js.float (float w)) (Js.float (float h));
    font.context##fillText glyph.str (Js.float 0.) (Js.float 0.);
    (*
    P.context##rect 0. 0. (float w) (float h);
    P.context##.strokeStyle := Js.string "1px solid black";
    P.context##stroke;
    *)

    (* Copy the data back out *)
    let copy =
      font.context##getImageData
        (Js.float 0.) (Js.float 0.) (Js.float (float w)) (Js.float (float h)) in

    let x_off = box.x0 in
    let y_off = box.y0 in
    for x=0 to w-1 do
        for y=0 to h-1 do
            let b = Dom_html.pixel_get copy##.data (x*4+3 + y*4*w) in
            Buffer.set data (x + x_off + (y + y_off)*width) b
        done
    done;
;;

let scale_for_mapping_em_to_pixels _font scale = 
    scale /. g_scale

let find _font (codepoint : int) = 
    let str : Js.js_string Js.t = Js.Unsafe.meth_call 
        Js.Unsafe.(pure_js_expr "String") 
        "fromCodePoint" 
        [|Js.Unsafe.inject codepoint|] 
    in
    Some { str; codepoint }
