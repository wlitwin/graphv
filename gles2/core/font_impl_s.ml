module type S = sig
        type t
        type iter
        type font = int
        module GlyphBitmap : sig
            include Flags.S
            type t
            val optional : t
            val required : t
        end

        type data

        val create : unit -> t

        module Quad : sig
            type t = {
                mutable x0 : float;
                mutable y0 : float;
                mutable s0 : float;
                mutable t0 : float;
                mutable x1 : float;
                mutable y1 : float;
                mutable s1 : float;
                mutable t1 : float;
            }

            val empty : unit -> t
        end

        module Iter : sig
            val next_x : iter -> float
            val codepoint : iter -> int
            val start : iter -> int
            val end_ : iter -> int
            val next : iter -> int
            val x : iter -> float
            val y : iter -> float
        end

        type v_metrics = {
            ascender : float;
            descender : float;
            line_height : float;
        }

        val bounds : t -> float -> float -> ?off:int -> ?end_:int -> string -> (float * Bounds.t)
        val line_bounds : t -> float -> (float * float)
        val set_size : t -> float -> unit
        val set_spacing : t -> float -> unit
        val set_blur : t -> float -> unit
        val set_align : t -> Align.t -> unit
        val set_font : t -> int -> unit
        val vert_metrics : t -> v_metrics
        val find_font : t -> string -> font option

        val add_font : t -> string -> string -> font option

        val validate_texture : t -> (int * int * int * int) option
        val get_texture_data : t -> data * int * int

        val iter_init : t -> float -> float -> ?start:int -> ?end_:int -> string -> GlyphBitmap.t -> iter
        val iter_next : t -> iter -> Quad.t -> bool

        val add_fallback_id : t -> font:font -> fallback:font -> unit
        val add_fallback : t -> name:string -> fallback:string -> unit
        val reset_fallback_id : t -> font:font -> unit
        val reset_fallback : t -> name:string -> unit
end
