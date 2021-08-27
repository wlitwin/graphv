open Graphv_core_lib

module ErrorCode = struct
    type t = Atlas_full
           | Scratch_full
           | States_overflow
           | States_underflow
end

module Params = struct
    type t = {
        width  : int;
        height : int;
    }
end

module Make(Impl : FontBackend.S) : Graphv_core_lib.Font_impl.S 
    with type data = Impl.Buffer.t
= struct

module Quad = Quad

module GlyphBitmap = GlyphBitmap

module Glyph = struct
    type t = {
        codepoint : int;
        size : float;
        blur : float;
        x0 : float;
        y0 : float;
        x1 : float;
        y1 : float;
        xadv : float;
        xoff : float;
        yoff : float;
        tt_glyph : Impl.glyph;
    }

    let empty = {
        codepoint = 0;
        size = 0.;
        blur = 0.;
        x0 = 0.;
        y0 = 0.;
        x1 = 0.;
        y1 = 0.;
        xadv = 0.;
        xoff = 0.;
        yoff = 0.;
        tt_glyph = Impl.invalid_glyph;
    }
end

module LUT = struct
    type key = {
        mutable codepoint : float;
        mutable size : float;
        mutable blur : float;
    }

    let hash (v : key) =
        let c = int_of_float v.codepoint in
        let b = int_of_float v.blur in
        let s = int_of_float v.size in
        let h = 17 in
        let h = h * 31 + c in
        let h = h * 31 + s in
        h * 31 + b

    let equal (a : key) (b : key) : bool =
        if Int.equal (int_of_float a.codepoint) (int_of_float b.codepoint) then (
            if Int.equal (int_of_float a.size) (int_of_float b.size) then (
                Int.equal (int_of_float a.blur) (int_of_float b.blur)
            ) else false
        ) else false

    include (Hashtbl.Make[@inlined](struct
            type t = key
            let hash = hash
            let equal = equal
        end) : Hashtbl.S with type key := key)

    let [@inline always] replace t c s b v =
        let lookup_key = {
            codepoint = float c;
            size = s*.10.;
            blur = b;
        } in
        replace t lookup_key v
    ;;

    let lookup_key = {
        codepoint = 0.;
        size = 0.;
        blur = 0.;
    }

    let [@inline always] find_opt t c s b =
        lookup_key.codepoint <- float c;
        lookup_key.size <- s*.10.;
        lookup_key.blur <- b;
        find_opt t lookup_key
end

module StringTbl = struct
    include Hashtbl.Make(struct
        type t = string
        let hash v = Hashtbl.hash v
        let equal a b = String.equal a b
    end)
end

module IntTbl = struct
    include Hashtbl.Make(struct
        type t = int 
        let hash v = v*13
        let equal a b = Int.equal a b
    end)
end

module Font = struct
    type t = {
        id : int;
        font : Impl.font;
        name : string;
        ascender : float;
        descender : float;
        line_height : float;
        glyphs : Glyph.t DynArray.t;
        lut : Glyph.t LUT.t;
        fallback : int DynArray.t;
    }
end

module TextIter = struct
    type t = {
        mutable x : float;
        mutable y : float;
        mutable next_x : float;
        scale : float;
        spacing : float;
        font : Font.t;
        mutable code_point : int;
        size : float;
        blur : float;
        mutable prev_glyph : Glyph.t;
        str : string;
        mutable start : int;
        mutable next : int;
        end_ : int;
        mutable utf8_state : int;
        bitmap_option : GlyphBitmap.t;
    }

    let x t = t.x
    let y t = t.y
    let next t = t.next
    let next_x t = t.next_x
    let start t = t.start
    let end_ t = t.end_
    let codepoint t = t.code_point
end

module FontState = struct
    type t = {
        mutable font : int;
        mutable align : Align.t;
        mutable size : float;
        mutable blur : float;
        mutable spacing : float;
    }

    let create () = {
        size = 12.0;
        font = 0;
        blur = 0.;
        spacing = 0.;
        align = Align.(or_ left baseline);
    }
end

(*
let hash_int a =
    let a = a + lnot (a lsl 15) in
    let a = a lxor (a lsr 10) in
    let a = a + (a lsl 3) in
    let a = a lxor (a lsr 6) in
    let a = a + lnot (a lsl 11) in
    let a = a lxor (a lsr 16) in
    a
;;
*)

let utf8_accept = 0

let utf8d = [|
    (* The first part of the table maps bytes to character classes that *)
    (* to reduce the size of the transition table and create bitmasks. *)
    0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;
    7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;  7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;
    8;8;2;2;2;2;2;2;2;2;2;2;2;2;2;2;  2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;
    10;3;3;3;3;3;3;3;3;3;3;3;3;4;3;3; 11;6;6;6;5;8;8;8;8;8;8;8;8;8;8;8;

    (* The second part is a transition table that maps a combination *)
    (* of a state of the automaton and a character class to a state. *)
    0;12;24;36;60;96;84;12;12;12;48;72; 12;12;12;12;12;12;12;12;12;12;12;12;
    12; 0;12;12;12;12;12; 0;12; 0;12;12; 12;24;12;12;12;12;12;24;12;24;12;12;
    12;12;12;12;12;12;12;24;12;12;12;12; 12;24;12;12;12;12;12;12;12;24;12;12;
    12;12;12;12;12;12;12;36;12;36;12;12; 12;36;12;12;12;12;12;36;12;36;12;12;
    12;36;12;12;12;12;12;12;12;12;12;12;
|]

let decutf8 state codep byte =
    let typ = utf8d.(byte) in

    let value = 
        if !state <> utf8_accept then (
            (byte land 0x3f) lor (!codep lsl 6)
        ) else (
            (0xff lsr typ) land byte
        )
    in
    codep := value;
    state := utf8d.(256 + !state + typ);
    !state
;;

type t = {
    params : Params.t;
    itw : float;
    ith : float;
    tex_data : Impl.Buffer.t;
    mutable dirty_rect : int * int * int * int;
    fonts : Font.t IntTbl.t;
    fonts_by_name : Font.t StringTbl.t;
    mutable next_font_id : int;
    atlas : Atlas.t;
    scratch : char DynArray.t;
    states : FontState.t DynArray.t; 
}

let get_state t =
    DynArray.last t.states
;;

let reset_fallback t ~name =
    let base = StringTbl.find t.fonts_by_name name in
    DynArray.clear base.fallback;
    DynArray.clear base.glyphs;
    LUT.clear base.lut;
;;

let reset_fallback_id t ~font =
    let base = IntTbl.find t.fonts font in
    DynArray.clear base.fallback;
    DynArray.clear base.glyphs;
    LUT.clear base.lut;
;;

let add_fallback_id t ~font ~fallback =
    let base = IntTbl.find t.fonts font in
    DynArray.add base.fallback fallback
;;

let add_fallback t ~name ~fallback =
    let base = StringTbl.find t.fonts_by_name name in
    let fallback = StringTbl.find t.fonts_by_name fallback in
    DynArray.add base.fallback fallback.id
;;

let set_size t size =
    (get_state t).size <- size

let set_spacing t spacing =
    (get_state t).spacing <- spacing

let set_blur t blur =
    (get_state t).blur <- blur

let set_align t align =
    (get_state t).align <- align

let set_font t font =
    (get_state t).font <- font

let create (params : Params.t) =
    let base_state = FontState.create() in
    let t =
        {
            params;
            scratch = DynArray.create 100 Char.(chr 0);
            atlas = Atlas.create ~width:params.width ~height:params.height ~ncount:256;
            fonts = IntTbl.create 4;
            next_font_id = 0;
            itw = 1. /. float params.width; 
            ith = 1. /. float params.height;
            tex_data = Impl.Buffer.create (params.width * params.height);
            dirty_rect = (params.width, params.height, 0, 0); 
            states = DynArray.create 1 FontState.(create());
            fonts_by_name = StringTbl.create 4;
        }
    in
    DynArray.add t.states base_state;
    t
;;

let create () =
    create Params.{width=2048; height=2048;}
;;

let add_font_internal t name font =
    let vmetrics = Impl.vmetrics font in
    let ascent = Impl.VMetrics.ascent vmetrics in
    let descent = Impl.VMetrics.descent vmetrics in
    let line_gap = Impl.VMetrics.line_gap vmetrics in

    let ascent = ascent + line_gap in
    let fh = float (ascent - descent) in

    let ascender = float ascent /. fh in
    let descender = float descent /. fh in

    let lut = LUT.create 1024 in
    let font_id = t.next_font_id in
    t.next_font_id <- t.next_font_id + 1;
    let f = Font.{
        id = font_id;
        font;
        name;
        lut;
        ascender;
        descender;
        line_height = ascender -. descender;
        glyphs = DynArray.create 10 Glyph.empty;
        fallback = DynArray.create 10 ~-1;
    } in
    IntTbl.replace t.fonts font_id f;
    StringTbl.replace t.fonts_by_name name f;
    font_id
;;

let add_font t name path =
    let font = Impl.load_font path in
    Option.map (add_font_internal t name) font
;;

let build_glyph_bitmap font glyph scale =
    let metrics = Impl.hmetrics font glyph in
    let box = Impl.get_glyph_bitmap_box font glyph ~scale in
    metrics, box
;;

let get_glyph_size font size glyph =
    let scale = Impl.scale_for_mapping_em_to_pixels font size in
    let m, box = build_glyph_bitmap font glyph scale in
    scale, float (Impl.HMetrics.advance_width m), box
;;

let lookup_glyph t (font : Font.t) codepoint =
    let glyph = Impl.find font.font codepoint in
    match glyph with
    | Some g -> Some (font, g)
    | None ->
        let rec loop idx = 
            if idx >= DynArray.length font.fallback then None
            else (
                let font = IntTbl.find t.fonts DynArray.(get font.fallback idx) in
                let g = Impl.find font.font codepoint in
                match g with
                | None -> loop (idx+1)
                | Some g -> Some (font, g)
            )
        in
        loop 0
;;

let aprec = 16
let zprec = 7

let blur_cols (t : t) (off : int) (w : int) (h : int) (stride : int) (alpha : int) =
    let off = ref off in
    for _=0 to h-1 do
        let z = ref 0 in
        for x=1 to w-1 do
            let v = Impl.Buffer.get t.tex_data (!off+x) in
            let v = (alpha * ((v lsl zprec) - !z)) asr aprec in
            z := !z + v;
            Impl.Buffer.set t.tex_data (!off+x) (!z asr zprec)
        done;
        Impl.Buffer.set t.tex_data (!off+w-1) 0;
        z := 0;
        for x=w-2 downto 0 do
            let v = Impl.Buffer.get t.tex_data (!off+x) in
            let v = (alpha * ((v lsl zprec) - !z)) asr aprec in
            z := !z + v;
            Impl.Buffer.set t.tex_data (!off+x) (!z asr zprec)
        done;
        Impl.Buffer.set t.tex_data !off 0;
        off := !off + stride;
    done
;;

let blur_rows (t : t) (off : int) (w : int) (h : int) (stride : int) (alpha : int) =
    let off = ref off in
    let max = h*stride in
    for _=0 to w-1 do
        let z = ref 0 in
        let rec loop y =
            if y < max then (
                let v = Impl.Buffer.get t.tex_data (!off+y) in
                let v = (alpha * ((v lsl zprec) - !z)) asr aprec in
                z := !z + v;
                Impl.Buffer.set t.tex_data (!off+y) (!z asr zprec);
                loop (y+stride)
            ) else ()
        in
        loop stride;
        Impl.Buffer.set t.tex_data (!off + (h-1)*stride) 0;
        z := 0;
        let rec loop y =
            if y >= 0 then (
                let v = Impl.Buffer.get t.tex_data (!off+y) in
                let v = (alpha * ((v lsl zprec) - !z)) asr aprec in
                z := !z + v;
                Impl.Buffer.set t.tex_data (!off+y) (!z asr zprec);
                loop (y-stride)
            ) else ()
        in
        loop ((h-2)*stride);
        Impl.Buffer.set t.tex_data !off 0;
        incr off;
    done
;;

let do_blur (t : t) (off : int) (w : int) (h : int) (width : int) (blur : int) =
    if blur > 0 then (
        let sigma = float blur *. 0.57735 in
        let alpha = (float (1 lsl aprec)) *. (1. -. (Float.exp (-2.3 /. (sigma +. 1.)))) |> int_of_float in
        blur_rows t off w h width alpha;
        blur_cols t off w h width alpha;
        blur_rows t off w h width alpha;
        blur_cols t off w h width alpha;
    )
;;

let build_glyph (t : t) codepoint (orig_font : Font.t) (font : Font.t) stb_glyph size pad blur bitmap_option =
    (* We may have found no glyph, or fallback glyph, will cache empty glyph *)
    let scale, advance, box = get_glyph_size font.font size stb_glyph in
    let gw = Impl.Box.(x1 box - x0 box + pad*2) in
    let gh = Impl.Box.(y1 box - y0 box + pad*2) in

    let build_glyph gx gy =
        Glyph.{
            x0 = float gx;
            y0 = float gy;
            x1 = float (gx + gw);
            y1 = float (gy + gh);
            codepoint;
            xadv = Float.floor (scale *. advance *. 10.0);
            size;
            blur;
            xoff = float (Impl.Box.x0 box - pad);
            yoff = float (Impl.Box.y0 box - pad);
            tt_glyph = stb_glyph;
        }
    in

    match GlyphBitmap.to_pattern bitmap_option with
    | Optional -> Some (build_glyph ~-1 ~-1)
    | Required ->  
        match Atlas.add_rect t.atlas gw gh with
        | None -> None
        | Some (gx, gy) ->
            let new_glyph = build_glyph gx gy in

            let box = Impl.Box.create
                (gx + pad)
                (gy + pad)
                (gx + gw - pad)
                (gy + gh - pad)
            in

            (try
                Impl.make_glyph_bitmap 
                    font.font 
                    t.tex_data
                    ~width:t.params.width
                    ~height:t.params.height
                    ~scale
                    box 
                    stb_glyph;
            with _ ->
                Printf.printf "x0:%d y0:%d x1:%d y1:%d (w %d) (h %d)\n(gx %d) (gy %d) (gw %d) (gh %d)\nscale %.2f w %d h %d pad %d '%c'\n%!"
                    (Impl.Box.x0 box)
                    (Impl.Box.y0 box)
                    (Impl.Box.x1 box)
                    (Impl.Box.y1 box)
                    Impl.Box.(x1 box - x0 box)
                    Impl.Box.(y1 box - y0 box)
                    gx
                    gy
                    gw
                    gh
                    scale
                    t.params.width
                    t.params.height
                    pad
                    (Char.chr new_glyph.codepoint);
                failwith "EXN"
            );

            if blur > 0. then (
                let off = (int_of_float new_glyph.x0 + int_of_float new_glyph.y0 * t.params.width) in
                do_blur t off gw gh t.params.width (int_of_float blur);
            );

            let int = int_of_float in
            let x, y, w, h = t.dirty_rect in
            let x0 = min x (int new_glyph.x0) in
            let y0 = min y (int new_glyph.y0) in
            let x1 = max w (int new_glyph.x1) in
            let y1 = max h (int new_glyph.y1) in
            t.dirty_rect <- x0, y0, x1, y1;

            (* Cache in both the found font and the fallback font *)
            LUT.replace font.lut codepoint size blur new_glyph;
            LUT.replace orig_font.lut codepoint size blur new_glyph;

            Some new_glyph
;;

let get_glyph (t : t) (font : Font.t) (codepoint : int) (size : float) 
                (blur : float) (bitmap_option : GlyphBitmap.t) : Glyph.t option 
                =
    if size < 2. then None
    else (
        let size = size *. 0.1 in
        let blur = if blur > 20. then 20. else Float.floor blur in

        (* TUPLE allocation *)
        let glyph = LUT.find_opt font.lut codepoint size blur in
        match glyph with
        | Some g as ret when bitmap_option = GlyphBitmap.optional || (g.x0 >= 0. && g.y0 >= 0.) -> ret
        | _ ->
            let pad = (int_of_float blur)+2 in
            (* Create a new glyph or rasterize bitmap data for a cached glyph *)
            let glyph_font = lookup_glyph t font codepoint in
            let orig_font = font in
            match glyph_font with
            | None -> 
                build_glyph t codepoint orig_font font Impl.invalid_glyph size pad blur bitmap_option
            | Some (font, stb_glyph) -> 
                build_glyph t codepoint orig_font font stb_glyph size pad blur bitmap_option
    )
;;

let get_quad t font (prev_glyph : Glyph.t) (glyph : Glyph.t) ~scale ~spacing ~x ~y (quad : Quad.t) =
    let x = ref x in
    if Impl.is_invalid_glyph prev_glyph.tt_glyph = false then (
        let adv = float (Impl.kern_advance font prev_glyph.tt_glyph glyph.tt_glyph) *. scale in
        x := !x +. Float.floor (adv +. spacing +. 0.5);
    );

    let xoff = Float.floor (glyph.xoff +. 1.) in
    let yoff = Float.floor (glyph.yoff +. 1.) in
    let x0 = glyph.x0 +. 1. in
    let y0 = glyph.y0 +. 1. in
    let x1 = glyph.x1 -. 1. in
    let y1 = glyph.y1 -. 1. in

    let rx = Float.floor (!x +. xoff) in
    x := !x +. Float.floor (glyph.xadv /. 10.0 +. 0.5);
    let ry = Float.floor (y +. yoff) in

    quad.x0 <- rx;
    quad.y0 <- ry;
    quad.x1 <- rx +. x1 -. x0;
    quad.y1 <- ry +. y1 -. y0;

    quad.s0 <- x0 *. t.itw;
    quad.t0 <- y0 *. t.ith;
    quad.s1 <- x1 *. t.itw;
    quad.t1 <- y1 *. t.ith;

    !x
;;

let validate_texture (t : t) =
    let x0, y0, x1, y1 = t.dirty_rect in
    if x0 < x1 && y0 < y1 then (
        t.dirty_rect <- t.params.width, t.params.height, 0, 0;
        Some (x0, y0, x1, y1)
    ) else (
        None
    )
;;

let get_texture_data t =
    t.tex_data, t.params.width, t.params.height
;;

let get_vert_align (font : Font.t) align size =
    if Align.has align ~flag:Align.top then (
        font.ascender *. (Float.floor size *. 0.1)
    ) else if Align.has align ~flag:Align.middle then (
        (font.ascender +. font.descender) *. 0.5 *. (Float.floor size *. 0.1)
    ) else if Align.has align ~flag:Align.baseline then (
        0.
    ) else if Align.has align ~flag:Align.bottom then (
        font.descender *. (Float.floor size *. 0.1)
    ) else (
        0.
    )
;;

let tb_quad = Quad.empty()

let text_bounds t x y str start end_ =
    let state = get_state t in
    let isize = Float.floor (state.size*.10.) in
    let blur = Float.floor state.blur in
    let font = IntTbl.find t.fonts state.font in
    let scale = Impl.scale_for_mapping_em_to_pixels font.font state.size in

    let y = y +. get_vert_align font state.align isize in

    let minx = ref x in
    let miny = ref y in
    let maxx = ref x in
    let maxy = ref y in

    let startx = ref x in
    let x = ref x in
    let end_ = 
        match end_ with
        | None -> String.length str
        | Some end_ ->
            max 0 (min end_ String.(length str))
    in

    Quad.reset tb_quad;

    let start = ref start in
    let utf8_state = ref 0 in
    let codepoint = ref 0 in
    let prev_glyph_index = ref Glyph.empty in
    while !start <> end_ do
        if decutf8 utf8_state codepoint String.(get str !start |> Char.code) = 0 then (
            let glyph = get_glyph t font !codepoint isize blur GlyphBitmap.optional in
            match glyph with
            | None -> prev_glyph_index := Glyph.empty
            | Some (glyph : Glyph.t) ->
                let xn = get_quad t font.font !prev_glyph_index glyph ~scale ~spacing:state.spacing ~x:!x ~y tb_quad in
                x := xn;
                if tb_quad.x0 < !minx then (minx := tb_quad.x0);
                if tb_quad.x1 > !maxx then (maxx := tb_quad.x1);
                if tb_quad.y0 < !miny then (miny := tb_quad.y0);
                if tb_quad.y1 > !maxy then (maxy := tb_quad.y1);
                prev_glyph_index := glyph
        );

        incr start;
    done;

    let advance = !x -. !startx in

    if Align.has state.align ~flag:Align.right then (
        minx := !minx -. advance;
        maxx := !maxx -. advance;
    ) else if Align.has state.align ~flag:Align.center then (
        minx := !minx -. advance*.0.5;
        maxx := !maxx -. advance*.0.5;
    );

    advance, Bounds.{xmin = !minx; ymin = !miny; xmax = !maxx; ymax = !maxy}
;;

let line_bounds t y =
    let state = get_state t in
    let font = IntTbl.find t.fonts state.font in
    let size = state.size in

    let y = y +. get_vert_align font state.align (Float.floor (size*.10.)) in

    let miny = y -. font.ascender *. size in
    let maxy = miny +. font.line_height *. size in
    miny, maxy
;;

let iter_init t x y ?(start=0) ?end_ str bitmap =
    let state = get_state t in
    let font = IntTbl.find t.fonts state.font in

    (* alignment *)
    let x =
        if Align.has state.align ~flag:Align.right then (
            let w, _ = text_bounds t x y str start end_ in
            x -. w
        ) else if Align.has state.align ~flag:Align.center then (
            let w, _ = text_bounds t x y str start end_ in
            x -. w*.0.5
        ) else (
            x
        )
    in

    let size = Float.floor (state.size *. 10.) in
    let y = y +. get_vert_align font state.align size in

    let len = String.length str in

    let end_ = 
        match end_ with
        | None -> len
        | Some e -> 
                if e < 0 then 0 
                else if e > len then len
                else e
    in

    let iter : TextIter.t = {
        font;
        size = size;
        blur = Float.floor state.blur;
        scale = Impl.scale_for_mapping_em_to_pixels font.font (size /. 10.);
        x; y;
        next_x = x;
        spacing = state.spacing;
        next = start;
        str;
        start;
        end_;
        code_point = 0;
        prev_glyph = Glyph.empty;
        utf8_state = 0;
        bitmap_option = bitmap;
    } in
    iter
;;

let iter_next (t : t) (iter : TextIter.t) quad =
    Quad.reset quad;

    iter.start <- iter.next;

    if iter.next = iter.end_ then (
        false
    ) else (
        let state = ref iter.utf8_state in
        let codepoint = ref iter.code_point in
        let rec loop start =
            if Int.equal start iter.end_ then (
                start
            ) else (
                let ch = String.get iter.str start in
                if decutf8 state codepoint (ch |> Char.code) = 0 then (
                    let start = start+1 in
                    iter.code_point <- !codepoint;
                    iter.utf8_state <- !state;

                    iter.x <- iter.next_x;

                    let glyph = get_glyph t iter.font iter.code_point iter.size iter.blur iter.bitmap_option in
                    begin match glyph with
                    | None -> 
                        iter.prev_glyph <- Glyph.empty;
                        loop start
                    | Some (glyph : Glyph.t) ->
                        let next_x = get_quad t iter.font.font iter.prev_glyph glyph 
                            ~scale:iter.scale ~spacing:iter.spacing ~x:iter.next_x ~y:iter.y
                            quad
                        in
                        iter.next_x <- next_x;
                        iter.prev_glyph <- glyph;
                        start
                    end;
                ) else (
                    iter.code_point <- !codepoint;
                    iter.utf8_state <- !state;
                    loop (start+1)
                );
            )
        in
        let start = loop iter.next in
        iter.next <- start;
        true
    )
;;

let find_font (t : t) name =
    StringTbl.find_opt t.fonts_by_name name
;;

type v_metrics = {
    ascender : float;
    descender : float;
    line_height : float;
}

let vert_metrics (t : t) =
    let state = get_state t in
    let font = IntTbl.find t.fonts state.font in
    let size = Float.floor (state.size*.10.) in

    {ascender = font.ascender*.size/.10.;
     descender = font.descender*.size/.10.;
     line_height = font.line_height*.size/.10.;
    }
;;

type font = int
module Iter = TextIter
type iter = TextIter.t
type data = Impl.Buffer.t

let bounds t x y ?(off=0) ?end_ str =
    text_bounds t x y str off end_

let find_font t f =
    let res = find_font t f in
    match res with
    | None -> None
    | Some font -> Some font.id

end
