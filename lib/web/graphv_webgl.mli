(** This module is ready for use by user applications. It combines the {!module:Graphv_webgl_impl} module with the {!module:Graphv_font_js} module to produce a new vector library.
*)

open Js_of_ocaml

(** Flags for rendering quality. *)
module CreateFlags : sig
  type t
  val no_flags : t
  val has : t -> flag:t -> bool
  val or_ : t -> t -> t
  val ( lor ) : t -> t -> t
  val remove : t -> flag:t -> t

  (** Anti-alias edges by drawing an extra fringe outline. *)
  val antialias : t

  (** Use the stencil buffer to prevent artifacts for overlapping strokes. *)
  val stencil_strokes : t

  val debug : t
end

type t

(** Creates a new renderer from a WebGL context. Make sure the
    stencil buffer is enabled for the WebGL context.
 *)
val create : flags:CreateFlags.t -> WebGL.renderingContext Js.t -> t

(**
   Text alignment flags.
*)
module Align : sig
  type t = int
  val no_flags : t
  val has : t -> flag:t -> bool
  val or_ : t -> t -> t
  val ( lor ) : t -> t -> t

  val remove : t -> flag:t -> t

  (** Return only the set vertical values *)
  val v_align : t -> t

  (** Return only the set horizontal values *)
  val h_align : t -> t

  (** {2 Horizontal flags} *)

  val left : t
  val center : t
  val right : t

  (** {2 Vertical flags} *)

  val top : t
  val middle : t
  val bottom : t
  val baseline : t
end

(**
 Blending modes used for compositing. The current blend mode determines how a fill or stroke is composited to the current framebuffer. See {{:https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml} here} for more information on the different blending modes.
 *)
module BlendFactor : sig
    type t = Zero
           | One
           | Src_color
           | One_minus_src_color
           | Dst_color
           | One_minus_dst_color
           | Src_alpha
           | One_minus_src_alpha
           | Dst_alpha
           | One_minus_dst_alpha
           | Src_alpha_saturate
end

module Bounds : sig
  type t = {
    xmin : float;
    ymin : float;
    xmax : float;
    ymax : float;
  }
  val scale : t -> float -> t
  val empty : t
end

(** This module exposes the native platforms buffer. Can be used for copy-free interop. *)
module Buffer : sig
    module UByte :
      sig
        type t = Typed_array.uint8Array Js.t

        (** [set t off value] will set the [value] at offset [off] in the buffer. *)
        val set : t -> int -> int -> unit

        (** [get t off] will return the value at offset [off]. *)
        val get : t -> int -> int

        (** Return the number of elements in the buffer. *)
        val length : t -> int

        (** [sub t off len] will create a sub array starting at offset [off] with length [len]. This operation does not copy any array values. *)
        val sub : t -> int -> int -> t

        (** Creates a new buffer. The buffer is initialized with zeros. *)
        val create : int -> t

        (** The empty buffer. Can be used as a placeholder value. Has size 0. *)
        val empty : t
      end
    module Float :
      sig
        type t = Typed_array.float32Array Js.t

        (** [set t off value] will set the [value] at offset [off] in the buffer. *)
        val set : t -> int -> float -> unit

        (** [get t off] will return the value at offset [off]. *)
        val get : t -> int -> float

        (** Return the number of elements in the buffer. *)
        val length : t -> int

        (** Creates a new buffer. The buffer is initialized with zeros. *)
        val create : int -> t

        (** Copy one buffer into another.
            @param src The buffer to copy from.
            @param s_off The offset to start copying from the [src] buffer.
            @param dst The buffer to copy to.
            @param d_off The offset to start copying to the [dst] buffer.
            @param len The number of values to copy.
            *)
        val blit :
          src:t -> s_off:int -> dst:t -> d_off:int -> len:int -> unit

        (** Fill the buffer with the given value. *)
        val fill : t -> float -> unit
    end
end

(** Operations on colors. *)
module Color : sig
  type t = {
    r : float;
    g : float;
    b : float;
    a : float;
  }

  (** Multiple all colors by alpha:

      [r = r*a]

      [g = g*a]

      [b = b*a]

      [a = a]
  *)
  val premultiply : t -> t

  (** Assumes all values are in the 0-1 range *)
  val rgbaf : r:float -> g:float -> b:float -> a:float -> t

  (** Assumes all values are in the 0-1 range *)
  val rgbf : r:float -> g:float -> b:float -> t

  (** Assumes all values are in the 0-255 range *)
  val rgba : r:int -> g:int -> b:int -> a:int -> t

  (** Assumes all values are in the 0-255 range *)
  val rgb : r:int -> g:int -> b:int -> t

  (** [r=255 g=255 b=255 a=255] *)
  val white : t

  (** [r=0 g=0 b=0 a=255] *)
  val black : t

  (** [r=0 g=0 b=0 a=0] *)
  val transparent : t

  (** Linearly interpolates between two colors.

      [0 <= a <= 1]
  *)
  val lerp : t -> t -> a:float -> t

  (** Clamp value min max *)
  val clamp : float -> float -> float -> float

  (** Set the alpha value, assumes 0-1 range *)
  val transf : t -> float -> t

  (** Set the alpha value, assumes 0-255 range *)
  val trans : t -> int -> t

  (** Create a color from HSL, alpha = 255

      [0 <= h <= 1]

      [0 <= s <= 1]

      [0 <= l <= 1]
      *)
  val hsl : h:float -> s:float -> l:float -> t

  (** Create a color from HSL with alpha

      [0 <= h <= 1]

      [0 <= s <= 1]

      [0 <= l <= 1]

      [0 <= a <= 255]
  *)
  val hsla : h:float -> s:float -> l:float -> a:int -> t
end

module CompositeOperation : sig
    type t = Source_over
           | Source_in
           | Source_out
           | Atop
           | Destination_over
           | Destination_in
           | Destination_out
           | Destination_atop
           | Lighter
           | Copy
           | Xor
end

module CompositeOperationState : sig
  type t = {
    src_rgb : BlendFactor.t;
    dst_rgb : BlendFactor.t;
    src_alpha : BlendFactor.t;
    dst_alpha : BlendFactor.t;
  }

  val of_composite_operation : CompositeOperation.t -> t
end

module ImageFlags : sig
  type t
  val no_flags : t
  val generate_mipmaps : t
  val repeat_x : t
  val repeat_y : t
  val flip_y : t
  val premultiplied : t
  val nearest : t
  val ( lor ) : t -> t -> t
  val remove : t -> flag:t -> t
  val has : t -> flag:t -> bool
end

module LineCap : sig
    type t = Butt | Round | Square | Default
end

module LineJoin : sig
    type t = Miter | Bevel | Round
end

(** A module for basic 2x2 (+2) matrix operations. *)
module Matrix : sig
  (**
    A 2x2 transform matrix with 2 more values for translation.

    [m0 m1]

    [m2 m3]

    [m4 m5]
   *)
  type t = {
    mutable m0 : float; (** cos a, scale{_x} *)
    mutable m1 : float; (** sin a, shear{_x} *)
    mutable m2 : float; (** -sin a, shear{_y} *)
    mutable m3 : float; (** cos a, scale{_y} *)
    mutable m4 : float; (** translate{_x} *)
    mutable m5 : float; (** translate{_y} *)
  }
  val create : unit -> t
  val copy : t -> t
  val zero : t -> unit
  val translate : t -> x:float -> y:float -> unit

  (** The formula is:

      [sx = sqrt(m0*m0 + m2*m2)]

      [sy = sqrt(m1*m1 + m3*m3)]

      [(sx + sy) * 0.5]
  *)
  val get_average_scale : t -> float

  (** Standard matrix multiplication: [dst = dst*src] *)
  val multiply : dst:t -> src:t -> unit

  val transform_point : t -> float -> float -> float * float

  (** Reversed multiplication: [dst = src*dst] *)
  val premultiply : dst:t -> src:t -> unit
  val scale : t -> xs:float -> ys:float -> unit
  val inverse : dst:t -> src:t -> unit

  (** @param angle Angle in radians. *)
  val rotate : t -> angle:float -> unit

  val identity : t -> unit

  (** @param angle Angle in radians. *)
  val skew_x : t -> angle:float -> unit

  (** @param angle Angle in radians. *)
  val skew_y : t -> angle:float -> unit

  (** Returns an array:

    [m0 m1 0 0]

    [m2 m3 0 0]

    [m4 m5 1 0]
  *)
  val to_3x4 : t -> float array

  (** [m0*m3 - m2*m1 < 0] *)
  val is_flipped : t -> bool

  val print : t -> unit
end

module Winding : sig
    type t = CCW | CW
    val ccw : t
    val cw : t
end

(** A module for swapping the default integer operators with floating point operators. *)
module FloatOps : sig
  external ( + ) : float -> float -> float = "%addfloat"
  external ( - ) : float -> float -> float = "%subfloat"
  external ( / ) : float -> float -> float = "%divfloat"
  external ( * ) : float -> float -> float = "%mulfloat"
  val ( =. ) : int -> int -> bool
  val ( <. ) : int -> int -> bool
  val ( >. ) : int -> int -> bool
  val ( >=. ) : int -> int -> bool
  val ( <=. ) : int -> int -> bool
  val ( < ) : float -> float -> bool
  val ( > ) : float -> float -> bool
  val ( >= ) : float -> float -> bool
  val ( <= ) : float -> float -> bool
  val min : float -> float -> float
  val max : float -> float -> float
  val imin : int -> int -> int
  val imax : int -> int -> int
  val ( = ) : float -> float -> bool
  external ( +. ) : int -> int -> int = "%addint"
  external ( -. ) : int -> int -> int = "%subint"
  external ( *. ) : int -> int -> int = "%mulint"
  external ( /. ) : int -> int -> int = "%divint"
end

(** Saves all context state to be restored later. Must have a corresponding [restore] call in order to be correct. Saves and restores happen in LIFO (stack) order. *)
val save : t -> unit

(** Restore a previously saved state. Must have a previous [save] call in order to be correct. Saves and restores happen in LIFO (stack) order. *)
val restore : t -> unit

(** Reset the current state to default settings. Only affects the current state, not an previously saved states. Does not clear any saved states. *)
val reset : t -> unit

val set_device_pixel_ratio : t -> float -> unit
val set_shape_antialias : t -> enabled:bool -> unit
val set_miter_limit : t -> limit:float -> unit
val set_stroke_width : t -> width:float -> unit
val set_line_cap : t -> cap:LineCap.t -> unit
val set_line_join : t -> join:LineJoin.t -> unit

(** Start a fresh frame. Resets all state.

    @param width The width of the viewport area.
    @param height The height of the viewport area.
    @param device_ratio The scaling factor for the render. Also known as the Device Pixel Ratio. Use this to get pixel-perfect rendering on high-resolution displays.
*)
val begin_frame :
  t -> width:float -> height:float -> device_ratio:float -> unit

(** Cancels a built up frame. Should call {!val:begin_frame} to start a new render. *)
val cancel_frame : t -> unit

(** Flush and render the frame to the screen. Nothing is drawn until this method is called. Alternatively can call {!val:cancel_frame} to not render the build up frame. *)
val end_frame : t -> unit

module Transform :
  sig
    val reset : t -> unit
    val transform : t -> Matrix.t -> unit
    val translate : t -> x:float -> y:float -> unit
    val rotate : t -> angle:float -> unit
    val skew_x : t -> angle:float -> unit
    val skew_y : t -> angle:float -> unit
    val scale : t -> x:float -> y:float -> unit
    val current_transform : t -> Matrix.t
    val deg_to_rad : float -> float
    val rad_to_deg : float -> float
  end
module Scissor :
  sig
    val scissor : t -> x:float -> y:float -> w:float -> h:float -> unit
    val intersect : t -> x:float -> y:float -> w:float -> h:float -> unit
    val reset : t -> unit
  end
module Global :
  sig
    val set_composite_operation :
      t -> op:CompositeOperationState.t -> unit
    val set_composite_blend_func :
      t -> src:BlendFactor.t -> dst:BlendFactor.t -> unit
    val set_composite_blend_func_separate :
      t ->
      src_rgb:BlendFactor.t ->
      dst_rgb:BlendFactor.t ->
      src_alpha:BlendFactor.t -> dst_alpha:BlendFactor.t -> unit
    val set_alpha : t -> alpha:float -> unit
  end

(** Fill the last created path. You can call both stroke and fill for a given path. The path is not cleared until {!val:Path.begin_} is called again. *)
val fill : t -> unit

(** Stroke the last created path. You can call both stroke and fill for a given path. The path is not cleared until {!val:Path.begin_} is called again *)
val stroke : t -> unit

(** Draw shapes. *)
module Path :
  sig
    val begin_ : t -> unit
    val close : t -> unit
    val move_to : t -> x:float -> y:float -> unit
    val line_to : t -> x:float -> y:float -> unit
    val bezier_to :
      t ->
      c1x:float ->
      c1y:float -> c2x:float -> c2y:float -> x:float -> y:float -> unit
    val quad_to : t -> cx:float -> cy:float -> x:float -> y:float -> unit
    val rect : t -> x:float -> y:float -> w:float -> h:float -> unit
    val winding : t -> winding:Winding.t -> unit
    val arc :
      t ->
      cx:float ->
      cy:float ->
      r:float -> a0:float -> a1:float -> dir:Winding.t -> unit
    val arc_to :
      t ->
      x1:float ->
      y1:float -> x2:float -> y2:float -> radius:float -> unit
    val ellipse :
      t -> cx:float -> cy:float -> rx:float -> ry:float -> unit
    val circle : t -> cx:float -> cy:float -> r:float -> unit
    val rounded_rect :
      t -> x:float -> y:float -> w:float -> h:float -> r:float -> unit
    val rounded_rect_varying :
      t ->
      x:float ->
      y:float ->
      w:float ->
      h:float ->
      top_left:float ->
      top_right:float -> bot_left:float -> bot_right:float -> unit
  end
module Image :
  sig
    type image
    type data = Buffer.UByte.t
    val dummy : image
    val from_color :
      t ->
      data:Color.t array ->
      width:int -> height:int -> flags:ImageFlags.t -> image option
    val from_buffer :
      t ->
      data:data ->
      width:int -> height:int -> flags:ImageFlags.t -> image option
    val update_image : t -> image:image -> data:data -> bool
    val size : t -> image -> int * int
  end

(** Create various fill and stroke effects. *)
module Paint :
  sig
    type ctx = t
    type t
    val linear_gradient :
      ctx ->
      sx:float ->
      sy:float ->
      ex:float -> ey:float -> icol:Color.t -> ocol:Color.t -> t
    val box_gradient :
      ctx ->
      x:float ->
      y:float ->
      w:float ->
      h:float -> r:float -> f:float -> icol:Color.t -> ocol:Color.t -> t
    val radial_gradient :
      ctx ->
      cx:float ->
      cy:float ->
      in_radius:float ->
      out_radius:float -> icol:Color.t -> ocol:Color.t -> t
    val image_pattern :
      ctx ->
      cx:float ->
      cy:float ->
      w:float ->
      h:float -> angle:float -> image:Image.image -> alpha:float -> t
  end
val set_fill_color : t -> color:Color.t -> unit
val set_fill_paint : t -> paint:Paint.t -> unit
val set_stroke_color : t -> color:Color.t -> unit
val set_stroke_paint : t -> paint:Paint.t -> unit

(** Module for text rendering. Text is rendered using a glyph bitmap. The default bitmap
    size is 2048x2048. It is possible to run out of space in this bitmap so try to
    be conservative with font sizes and number of fonts. Glyphs are grouped by font, size,
and blur. A glyph will be rendered once per combination of properties.
*)
module Text :
  sig
    type font
    type bounds = { box : Bounds.t; advance : float; }

    (** Draw a text substring with the current font at the specified location.

        @param x The x location to draw the font at. Depending on the current
        alignment this parameter may represent the center, bottom left, right,
        etc.
        @param y The y location to draw the font at. Depending on the current
        alignment this parameter may represent the center, bottom, top, etc.
        @param start The starting index of the string. Defaults to 0.
        @param end_ The ending index. Defaults to the end of the string.
    *)
    val text :
      t ->
      x:float -> y:float -> ?start:int -> ?end_:int -> string -> unit

    (** Same as {!val: text} except it returns the rendered length.
     *)
    val text_w :
      t ->
      x:float -> y:float -> ?start:int -> ?end_:int -> string -> float

    val find_font : t -> name:string -> font option

    (** Set the font size in pixel units. Warning: If this font size has not
     been used yet the glyphs will be generated on demand. This may cause a
     performance hit on the first rendering at this size. Try to avoid using
     many different sizes. *)
    val set_size : t -> size:float -> unit

    (** Set the current blur, in pixel units *)
    val set_blur : t -> blur:float -> unit

    (** Set the spacing between lines *)
    val set_line_height : t -> height:float -> unit

    (** Set the spacing between letters *)
    val set_letter_spacing : t -> spacing:float -> unit

    (** Set the alignment for rendered text. This changes the x,y origin for
        the rendered text.
    *)
    val set_align : t -> align:Align.t -> unit

    (** Change the font by name *)
    val set_font_face : t -> name:string -> unit

    (** Change the font by id *)
    val set_font_face_id : t -> id:int -> unit

    (** Create a new font *)
    val create : t -> name:string -> file:string -> font option

    val bounds :
      t ->
      x:float -> y:float -> ?start:int -> ?end_:int -> string -> bounds

    type metrics = {
      ascender : float;
      descender : float;
      line_height : float;
    }

    val metrics : t -> metrics

    type text_row = {
      start_index : int;
      end_index : int;
      width : float;
      minx : float;
      maxx : float;
      next : int;
    }

    (** Layout lines of text in [break_width] size rows.

        @param break_width The maximum width of a row
        @param max_rows The maximum number of rows to layout. This allows the [lines]
        parameter to be used accross different calls to save allocations.
        @param start The starting index of the string to layout, defaults to 0.
        @param end_ The ending index of the string to layout, default to the end of the string.

        @return The number of rows created.
      *)
    val break_lines :
      t ->
      break_width:float ->
      max_rows:int -> ?start:int -> ?end_:int -> lines:text_row array -> string -> int

    (** Helper function to initialize an empty array of rows *)
    val make_empty_rows : int -> text_row array

    (** Used to get glyph positions within a rendered string. Can be used for selections
        or other screen based picking.
      *)
    type glyph_position = {
      index : int;
      x : float;
      min_x : float;
      max_x : float;
    }

    (** A {!type:glyph_position} initialized to all zeros. *)
    val empty_glyph_position : glyph_position

    val glyph_positions :
      t ->
      x:float ->
      y:float -> ?start:int -> ?end_:int -> glyphs:glyph_position array -> string -> int

    val text_box :
      t ->
      x:float ->
      y:float ->
      break_width:float -> ?start:int -> ?end_:int -> string -> unit

    val box_bounds :
      t ->
      x:float ->
      y:float ->
      break_width:float -> ?start:int -> ?end_:int -> string -> Bounds.t

    val add_fallback_id : t -> font:font -> fallback:font -> unit
    val add_fallback : t -> name:string -> fallback:string -> unit
    val reset_fallback_id : t -> font:font -> unit
    val reset_fallback : t -> name:string -> unit
  end
