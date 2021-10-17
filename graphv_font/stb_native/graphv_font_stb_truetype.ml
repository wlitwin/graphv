module VMetrics = struct
    open Stb_truetype
    type t = vmetrics
    let ascent t = t.ascent
    let descent t = t.descent
    let line_gap t = t.line_gap
end

module HMetrics = struct
    open Stb_truetype
    type t = hmetrics
    let advance_width t = t.advance_width
    let left_side_bearing t = t.left_side_bearing
end

module Box = struct
    open Stb_truetype
    type t = box
    let x0 t = t.x0
    let y0 t = t.y0
    let x1 t = t.x1
    let y1 t = t.y1
    let create x0 y0 x1 y1 = {
        x0; y0; x1; y1;
    }
end

module Buffer = struct
    type t = Stb_truetype.buffer

    let create size =
        let arr =
            Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size
        in
        Bigarray.Array1.fill arr 0;
        arr

    let get = Bigarray.Array1.get
    let set = Bigarray.Array1.set
    let length = Bigarray.Array1.dim
    let sub = Bigarray.Array1.sub
    let empty = create 0
end

type font = Stb_truetype.t
type glyph = Stb_truetype.glyph
let find = Stb_truetype.find
let invalid_glyph = Stb_truetype.invalid_glyph
let is_invalid_glyph g = (g = invalid_glyph)
let vmetrics = Stb_truetype.vmetrics
let hmetrics = Stb_truetype.hmetrics
let kern_advance = Stb_truetype.kern_advance
let scale_for_mapping_em_to_pixels = Stb_truetype.scale_for_mapping_em_to_pixels
let make_glyph_bitmap font buf ~width ~height ~scale box glyph = 
    Stb_truetype.make_glyph_bitmap font buf ~width ~height ~scale_x:scale ~scale_y:scale box glyph
let get_glyph_bitmap_box font glyph ~scale =
    Stb_truetype.get_glyph_bitmap_box font glyph ~scale_x:scale ~scale_y:scale
let create_font data =
    let offsets = Stb_truetype.enum data in
    Stb_truetype.init data List.(hd offsets) |> Graphv_core_lib.Utils.some_exn

let load_font path =
    let chan = Stdlib.open_in_bin path in
    let len = Stdlib.in_channel_length chan in
    let bytes = Bytes.create len in
    let data = Buffer.create len in
    Stdlib.really_input chan bytes 0 len;
    for i=0 to len-1 do
        Buffer.set data i (Bytes.get_uint8 bytes i)
    done;
    Some (create_font data)
;;
