module type S = sig
    type font
    type glyph
    val invalid_glyph : glyph

    module Buffer : Graphv_core_lib.Buffer.UByteS

    module VMetrics : sig
        type t
        val ascent : t -> int
        val descent : t -> int
        val line_gap : t -> int
    end

    module HMetrics : sig
        type t
        val advance_width : t -> int
        val left_side_bearing : t -> int
    end

    val vmetrics : font -> VMetrics.t
    val hmetrics : font -> glyph -> HMetrics.t

    module Box : sig
        type t
        val x0 : t -> int
        val y0 : t -> int
        val x1 : t -> int
        val y1 : t -> int
        val create : int -> int -> int -> int -> t
    end

    val get_glyph_bitmap_box : font -> glyph -> scale:float -> Box.t
    val kern_advance : font -> glyph -> glyph -> int
    val create_font : Buffer.t -> font
    val is_invalid_glyph : glyph -> bool
    val make_glyph_bitmap : font -> Buffer.t -> width:int -> height:int -> scale:float -> Box.t -> glyph -> unit
    val scale_for_mapping_em_to_pixels : font -> float -> float
    val find : font -> int -> glyph option

    val load_font : string -> font option
end  

