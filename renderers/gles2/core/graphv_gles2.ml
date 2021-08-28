open Graphv_core_lib

module Make(Gl : Ogl_intf.S) : Impl.S
        with type gl = Gl.t
        and module Buffer = Gl.Buffer
        and module Dyn = Gl.Dyn
        and module VertexBuffer = Gl.VertexBuffer
        and module Path = Gl.Path
= struct

module Buffer = Gl.Buffer
module Dyn = Gl.Dyn
module Path = Gl.Path
module VertexBuffer = Gl.VertexBuffer

type gl = Gl.t

module FragUniforms = FragUniforms.Make(struct
    type buffer = Gl.Buffer.Float.t
    let get = Gl.Buffer.Float.get
    let set = Gl.Buffer.Float.set
    let create = Gl.Buffer.Float.create
end)

module Texture = struct
    type t = {
        mutable id : int;
        mutable tex : Gl.texture option;
        mutable width : int;
        mutable height : int;
        mutable type_ : [`RGBA | `Alpha];
        mutable flags : ImageFlags.t;
    }

    let empty () = {
        id = 0;
        tex = None;
        width = 0;
        height = 0;
        type_ = `RGBA;
        flags = ImageFlags.no_flags;
    }

    let reset (t : t) id =
        t.id <- id;
        t.tex <- None;
        t.width <- 0;
        t.height <- 0;
        t.type_ <- `RGBA;
        t.flags <- ImageFlags.no_flags;
end

let nearest_pow2 num =
    let n = if num > 0 then num - 1 else 0 in
    let n = n lor n lsr 1 in
    let n = n lor n lsr 2 in
    let n = n lor n lsr 4 in
    let n = n lor n lsr 8 in
    let n = n lor n lsr 16 in
    let n = n + 1 in
    n
;;

let convert_blend_factor = function
    | BlendFactor.Zero -> Gl.zero
    | One -> Gl.one
    | Src_color -> Gl.src_color
    | One_minus_src_color -> Gl.one_minus_src_color
    | Dst_color -> Gl.dst_color
    | One_minus_dst_color -> Gl.one_minus_dst_color
    | Src_alpha -> Gl.src_alpha
    | One_minus_src_alpha -> Gl.one_minus_src_alpha
    | Dst_alpha -> Gl.dst_alpha
    | One_minus_dst_alpha -> Gl.one_minus_dst_alpha
    | Src_alpha_saturate -> Gl.src_alpha_saturate
;;

module Blend = struct
    type t = {
        src_rgb : Gl.blending_factor option;
        dst_rgb : Gl.blending_factor option;
        src_alpha : Gl.blending_factor option;
        dst_alpha : Gl.blending_factor option;
    }

    let empty = {
        src_rgb = None;
        dst_rgb = None;
        src_alpha = None;
        dst_alpha = None;
    }

    let equal a b = 
        a.src_rgb = b.src_rgb
        && a.dst_rgb = b.dst_rgb
        && a.src_alpha = b.src_alpha
        && a.dst_alpha = b.dst_alpha
    
    let of_composite_op_state (op : CompositeOperationState.t) = {
        src_rgb = Some (convert_blend_factor op.src_rgb);
        dst_rgb = Some (convert_blend_factor op.dst_rgb);
        src_alpha = Some (convert_blend_factor op.src_alpha);
        dst_alpha = Some (convert_blend_factor op.dst_alpha);
    }
end

module ShaderType = struct
    let fill_grad = 0.
    let fill_img = 1.
    let simple = 2.
    let img = 3.
end

module IPath = struct
    type t = {
        fill_offset : int;
        fill_count : int;
        stroke_offset : int;
        stroke_count : int;
    }

    let empty = {
        fill_offset = 0;
        fill_count = 0;
        stroke_offset = 0;
        stroke_count = 0;
    }
end

module Call = struct

    type type_ = Fill
               | Convex_fill
               | Stroke
               | Triangles

    type t = {
        mutable type_ : type_;
        mutable image : int;
        mutable path_count : int;
        mutable triangle_offset : int;
        mutable triangle_count : int;
        mutable uniform_offset : int;
        mutable blend_func : Blend.t;
        uniforms : FragUniforms.t;
        uniforms2 : FragUniforms.t;
        paths : IPath.t DynArray.t;
    }

    let create type_ = {
        type_ = type_;
        image = 0;
        path_count = 0;
        triangle_offset = 0;
        triangle_count = 0;
        uniform_offset = 0;
        blend_func = Blend.empty;
        uniforms = FragUniforms.create();
        uniforms2 = FragUniforms.create();
        (* TODO - could pool these globally and steal into individual arrays? *)
        paths = DynArray.create 10 IPath.empty;
    }

    let empty () =
        create Stroke

    let reset t type_ =
        t.type_ <- type_;
        t.image <- 0;
        t.path_count <- 0;
        t.triangle_offset <- 0;
        t.triangle_count <- 0;
        t.uniform_offset <- 0;
        t.blend_func <- Blend.empty;
        Gl.Buffer.Float.fill t.uniforms 0.;
        Gl.Buffer.Float.fill t.uniforms2 0.;
        DynArray.clear t.paths;
    ;;
end

    let check_error _ = ()
        (*Gl.check_error*)

    type t = {
        impl : Gl.t;
        shader : Gl.program;
        locs : Gl.locs;
        vert_buf : Gl.buffer_id;
        mutable edge_antialias : bool;
        (* textures *)
        textures : (int, Texture.t) Hashtbl.t;
        mutable texture_id : int;
        (* stencils *)
        mutable dummy_tex : int;
        (* view *)
        mutable view : Gl.Buffer.Float.t;
        frag_uniforms : FragUniforms.t;
        
        mutable flags : CreateFlags.t;
        calls : Call.t DynArray.t;
        paths : IPath.t DynArray.t;

        (* Cached state *)
        mutable bound_texture : Gl.texture option;
        mutable stencil_mask : int;
        mutable stencil_func : Gl.depth_function;
        mutable stencil_func_ref : int;
        mutable stencil_func_mask : int;
        mutable blend_func : Blend.t;
    }

    let find_texture_by_id t = Hashtbl.find_opt t.textures

    let edge_antialias t = t.edge_antialias

    let alloc_texture (t : t) =
        let tex = Texture.empty() in
        t.texture_id <- t.texture_id + 1;
        Texture.reset tex t.texture_id;
        Hashtbl.replace t.textures tex.id tex;
        tex
    ;;

    let bind_texture (t : t) (id : Gl.texture option) =
        if t.bound_texture <> id then (
            t.bound_texture <- id;
            Gl.bind_texture t.impl Gl.texture_2d id
        );
    ;;

    let stencil_mask (t : t) mask =
        if t.stencil_mask <> mask then (
            t.stencil_mask <- mask;
            Gl.stencil_mask t.impl mask
        )
    ;;

    let stencil_func t func ref mask =
        if t.stencil_func <> func
            || t.stencil_func_ref <> ref
            || t.stencil_func_mask <> mask 
        then (
            t.stencil_func <- func;
            t.stencil_func_ref <- ref;
            t.stencil_func_mask <- mask;
            Gl.stencil_func t.impl func ref mask
        )
    ;;

    let blend_func_separate t blend =
        if not Blend.(equal t.blend_func blend) then (
            t.blend_func <- blend;
            let int_val = Utils.some_exn in
            Gl.blend_func_separate 
                t.impl
                (int_val blend.src_rgb)
                (int_val blend.dst_rgb)
                (int_val blend.src_alpha)
                (int_val blend.dst_alpha)
        )
    ;;

    let create_texture t type_ width height flags (data : Buffer.UByte.t) =
        let tex = alloc_texture t in
        tex.flags <- flags;

        if nearest_pow2 width <> width || nearest_pow2 height <> height then (
            if ImageFlags.has tex.flags ~flag:ImageFlags.repeat_x
                || ImageFlags.has tex.flags ~flag:ImageFlags.repeat_y then (
                Printf.printf "Error: non-power of two texture, repeat X/Y unsupported\n%!";
                tex.flags <- ImageFlags.remove tex.flags ~flag:ImageFlags.repeat_x; 
                tex.flags <- ImageFlags.remove tex.flags ~flag:ImageFlags.repeat_y; 
            );
            if ImageFlags.has tex.flags ~flag:ImageFlags.generate_mipmaps then (
                Printf.printf "Error: non-power of two texture, mipmaps unsupported\n%!";
                tex.flags <- ImageFlags.remove tex.flags ~flag:ImageFlags.generate_mipmaps
            )
        );

        let v = (Gl.gen_textures t.impl 1).(0) in

        tex.tex <- Some v;
        tex.width <- width;
        tex.height <- height;
        tex.type_ <- type_;

        bind_texture t tex.tex;

        Gl.pixel_storei t.impl Gl.unpack_alignment 1;

        begin match tex.type_ with
        | `RGBA ->
            Gl.tex_image2d t.impl Gl.texture_2d 0 Gl.rgba width height 0 Gl.rgba Gl.unsigned_byte data
        | `Alpha ->
            Gl.tex_image2d t.impl Gl.texture_2d 0 Gl.luminance width height 0 Gl.luminance Gl.unsigned_byte data
        end;

        let mipmaps = ImageFlags.has tex.flags ~flag:ImageFlags.generate_mipmaps in
        let nearest = ImageFlags.has tex.flags ~flag:ImageFlags.nearest in
        let repeat_x = ImageFlags.has tex.flags ~flag:ImageFlags.repeat_x in
        let repeat_y = ImageFlags.has tex.flags ~flag:ImageFlags.repeat_y in

        let min_filter =
            match mipmaps, nearest with
            | true, true -> Gl.nearest_mipmap_nearest
            | true, false -> Gl.linear_mipmap_linear
            | false, true -> Gl.nearest
            | false, false -> Gl.linear
        in
        Gl.tex_parameteri_1 t.impl Gl.texture_2d Gl.texture_min_filter min_filter;

        let mag_filter = if nearest then Gl.nearest else Gl.linear in
        Gl.tex_parameteri_1 t.impl Gl.texture_2d Gl.texture_mag_filter mag_filter;

        let wrap_s = if repeat_x then Gl.repeat else Gl.clamp_to_edge in
        Gl.tex_parameteri_2 t.impl Gl.texture_2d Gl.texture_wrap_s wrap_s;

        let wrap_t = if repeat_y then Gl.repeat else Gl.clamp_to_edge in
        Gl.tex_parameteri_2 t.impl Gl.texture_2d Gl.texture_wrap_t wrap_t;

        Gl.pixel_storei t.impl Gl.unpack_alignment 4;

        if mipmaps then (
            Gl.generate_mipmap t.impl Gl.texture_2d;
        );

        check_error "create tex";
        bind_texture t None;

        tex.id
    ;;

    let create_texture t ~type_ ~w ~h ~flags ~data = 
        Some (create_texture t type_ w h flags data)

    let create ~(flags : CreateFlags.t) impl = 
        check_error "init";
    
        (* TODO - check for anti-alias *)
        match Gl.create_program impl with
        | None -> None
        | Some (shader, locs) ->
            check_error "create done";
            Gl.finish impl;
            let t = {
                impl;
                shader;
                locs;
                vert_buf = locs.vert_buf;
                stencil_mask = 0;
                edge_antialias = CreateFlags.has flags ~flag:CreateFlags.antialias;
                (* textures *)
                textures = Hashtbl.create 10;
                texture_id = 0;
                bound_texture = None;
                dummy_tex = 0;

                view = Gl.Buffer.Float.create 2;

                frag_uniforms = FragUniforms.create();

                flags;
                stencil_func = Gl.equal;
                stencil_func_ref = 0;
                stencil_func_mask = 0;
                blend_func = Blend.empty;

                (*verts = VertexBuffer.create();*)
                paths = DynArray.create 128 IPath.empty;
                calls = DynArray.init 128 Call.empty;
            } in

            t.dummy_tex <-
                    create_texture 
                    t 
                    ~type_:`Alpha 
                    ~w:1 
                    ~h:1 
                    ~flags:ImageFlags.no_flags 
                    ~data:Gl.(Buffer.UByte.create 4)
                |> Option.value ~default:0;

            check_error "create done";
            Some t
    ;;

    let delete_texture t ~image = 
        match find_texture_by_id t image with
        | None -> false
        | Some tex ->
            match tex.tex with
            | None -> true
            | Some id ->
                Gl.delete_textures t.impl [|id|];
                true
    ;;

    let update_texture t ~image ~x:_ ~y ~w:_ ~h ~data = 
        let tex = find_texture_by_id t image in
        match tex with
        | None -> 
                check_error "update_texture_1";
                false
        | Some tex ->
            bind_texture t tex.tex;
            Gl.pixel_storei t.impl Gl.unpack_alignment 1;
            
            let index =
                match tex.type_ with
                | `RGBA -> y*tex.width*4
                | `Alpha -> y*tex.width
            in

            (* GLES2 doesn't support x-offsets *)
            let x = 0 in
            let w = tex.width in
            (* Should not copy *)
            let len = Gl.Buffer.UByte.length data - index in
            let data = Gl.Buffer.UByte.sub data index len in

            begin match tex.type_ with
            | `RGBA -> 
                Gl.tex_sub_image2d t.impl Gl.texture_2d 0 x y w h Gl.rgba Gl.unsigned_byte data
            | `Alpha ->
                Gl.tex_sub_image2d t.impl Gl.texture_2d 0 x y w h Gl.luminance Gl.unsigned_byte data
            end;

            Gl.pixel_storei t.impl Gl.unpack_alignment 4;
            bind_texture t None;

            check_error "update_texture_2";
            true
    ;;

    let max_vertex_count (paths : Path.t DynArray.t) =
        let count = ref 0 in
        DynArray.iter paths ~f:(fun path ->
            let fill = VertexBuffer.Sub.(num_verts path.fill) in
            let stroke = VertexBuffer.Sub.(num_verts path.stroke) in
            count := !count
                + fill
                + stroke
        );
        !count
    ;;

    let get_texture_size t ~image =
        match find_texture_by_id t image with
        | None -> 0, 0
        | Some tex -> tex.width, tex.height
    ;;

    let viewport t ~width ~height ~dpi:_ = 
        Gl.Buffer.Float.set t.view 0 width;
        Gl.Buffer.Float.set t.view 1 height;
    ;;

    let cancel t =
        DynArray.clear t.paths;
        DynArray.clear t.calls;
    ;;

    let convert_paint t frag (paint : Paint.t) (scissor : Scissor.t) width fringe stroke_thr =
        FragUniforms.set_inner_color frag Color.(premultiply paint.inner_color);
        FragUniforms.set_outer_color frag Color.(premultiply paint.outer_color);

        let invxform = Matrix.create() in

        if scissor.extent_0 < ~-.0.5 || scissor.extent_1 < ~-.0.5 then (
            FragUniforms.set_scissor_ext frag 1. 1.;
            FragUniforms.set_scissor_scale frag 1. 1.;
        ) else (
            Matrix.inverse ~dst:invxform ~src:scissor.xform;
            FragUniforms.set_scissor_mat frag
                invxform.m0 invxform.m1 0. 0.
                invxform.m2 invxform.m3 0. 0.
                invxform.m4 invxform.m5 1. 0.;

            let open Matrix in
            let x = scissor.xform in
            let open FloatOps in
            let scale0 = (Float.sqrt (x.m0*x.m0 + x.m2*x.m2)) / fringe in
            let scale1 = (Float.sqrt (x.m1*x.m1 + x.m3*x.m3)) / fringe in
            FragUniforms.set_scissor_ext frag scissor.extent_0 scissor.extent_1;
            FragUniforms.set_scissor_scale frag scale0 scale1;
        );

        let x0, x1 = paint.extent in
        FragUniforms.set_extent frag x0 x1;

        FragUniforms.set_stroke_mult frag ((width*.0.5 +. fringe*.0.5) /. fringe);
        FragUniforms.set_stroke_thr frag stroke_thr;

        if paint.image <> 0 then (
            match find_texture_by_id t paint.image with
            | None -> ()
            | Some tex ->
                if ImageFlags.has tex.flags ~flag:ImageFlags.flip_y then (
                    let m1 = Matrix.create()
                    and m2 = Matrix.create() in
                    Matrix.translate m1 ~x:0. ~y:(x1 *. 0.5);
                    Matrix.multiply ~dst:m1 ~src:paint.xform;
                    Matrix.scale m2 ~xs:1. ~ys:~-.1.;
                    Matrix.multiply ~dst:m2 ~src:m1;
                    Matrix.translate m1 ~x:0. ~y:(x1 *. 0.5);
                    Matrix.multiply ~dst:m1 ~src:m2;
                    Matrix.inverse ~dst:invxform ~src:m1;
                ) else (
                    Matrix.inverse ~dst:invxform ~src:paint.xform
                );

                FragUniforms.set_type frag ShaderType.fill_img;
                let tex_type =
                    match tex.type_ with
                    | `RGBA -> if ImageFlags.has tex.flags ~flag:ImageFlags.premultiplied then 0. else 1.
                    | `Alpha -> 2.
                in
                FragUniforms.set_tex_type frag tex_type;
        ) else (
            FragUniforms.set_type frag ShaderType.fill_grad;
            FragUniforms.set_radius frag paint.radius;
            FragUniforms.set_feather frag paint.feather;
            Matrix.inverse ~dst:invxform ~src:paint.xform
        );

        FragUniforms.set_paint_mat frag
            invxform.m0 invxform.m1 0. 0.
            invxform.m2 invxform.m3 0. 0.
            invxform.m4 invxform.m5 1. 0.;
    ;;

    let render_fill t (paint : Paint.t) composite_op scissor fringe (bounds : Bounds.t) (paths : Path.t DynArray.t) (verts : VertexBuffer.t) =
        let call = DynArray.steal t.calls Call.empty in
        Call.reset call Call.Fill;

        let npaths = DynArray.length paths in
        call.triangle_count <- 4;
        call.path_count <- npaths;
        call.image <- paint.image;
        call.blend_func <- Blend.of_composite_op_state composite_op;

        if npaths = 1 && DynArray.(get paths 0).convex then (
            call.type_ <- Call.Convex_fill;
            call.triangle_count <- 0; (* Bounding box fill quad not needed for convex fill *)
        );

        (* Allocate vertices for all the paths *)
        DynArray.iter paths ~f:(fun path ->
            let fill_offset = ref 0 in
            let fill_count = ref 0 in
            let stroke_offset = ref 0 in
            let stroke_count = ref 0 in

            let nfill = VertexBuffer.Sub.num_verts path.fill in
            if nfill > 0 then (
                fill_offset := VertexBuffer.Sub.vertex_offset path.fill;
                fill_count := VertexBuffer.Sub.num_verts path.fill;
            );
            let nstroke = VertexBuffer.Sub.num_verts path.stroke in
            if nstroke > 0 then (
                stroke_offset := VertexBuffer.Sub.vertex_offset path.stroke;
                stroke_count := VertexBuffer.Sub.num_verts path.stroke;
            );

            DynArray.add call.paths {
                fill_offset = !fill_offset;
                fill_count = !fill_count;
                stroke_offset = !stroke_offset;
                stroke_count = !stroke_count;
            };
        );

        if call.type_ = Call.Fill then (
            (* Quad *)
            let offset = VertexBuffer.num_verts verts in
            call.triangle_offset <- offset;

            VertexBuffer.set verts offset 
                bounds.xmax
                bounds.ymax
                0.5
                1.0;

            VertexBuffer.set verts (offset+1)
                bounds.xmax
                bounds.ymin
                0.5
                1.;

            VertexBuffer.set verts (offset+2)
                bounds.xmin
                bounds.ymax
                0.5
                1.;

            VertexBuffer.set verts (offset+3)
                bounds.xmin
                bounds.ymin
                0.5
                1.;

            FragUniforms.set_type call.uniforms ShaderType.simple;
            FragUniforms.set_stroke_thr call.uniforms ~-.1.;
            (* Fill shader *)
            convert_paint t call.uniforms2 paint scissor fringe fringe ~-.1.;
        ) else (
            (* Fill shader *)
            convert_paint t call.uniforms paint scissor fringe fringe ~-.1.;
        )
    ;;

    let render_stroke t (paint : Paint.t) composite_op scissor fringe stroke_width (paths : Path.t DynArray.t) =
        let call = DynArray.steal t.calls Call.empty in
        Call.reset call Call.Stroke;

        call.image <- paint.image; 
        call.blend_func <- Blend.of_composite_op_state composite_op;

        call.triangle_count <- ~-1;
        call.triangle_offset <- ~-1;

        DynArray.iter paths ~f:(fun path ->
            let count = VertexBuffer.Sub.num_verts path.stroke in
            if count > 0 then (
                DynArray.add call.paths {
                    IPath.empty with
                    stroke_offset = VertexBuffer.Sub.vertex_offset path.stroke;
                    stroke_count = VertexBuffer.Sub.num_verts path.stroke;
                };
            )
        );

        if CreateFlags.has t.flags ~flag:CreateFlags.stencil_strokes then (
            convert_paint t call.uniforms paint scissor stroke_width fringe ~-.1.;
            convert_paint t call.uniforms2 paint scissor stroke_width fringe (1. -. 0.5/.255.);
        ) else (
            convert_paint t call.uniforms paint scissor stroke_width fringe ~-.1.
        )
    ;;

    let set_uniforms t uniforms image =
        Gl.uniform4fv t.impl t.locs.frag (FragUniforms.as_array uniforms);
        
        let tex = 
            if Int.(not (equal image 0)) then (
                match find_texture_by_id t image with
                | Some _ as s -> s
                | None ->
                    find_texture_by_id t t.dummy_tex
            ) else None
        in

        let tex =
            match tex with
            | None as n -> n
            | Some t -> t.tex
        in

        bind_texture t tex;
        check_error "tex paint tex"
    ;;

    let stroke t (call : Call.t) =
        if CreateFlags.has t.flags ~flag:CreateFlags.stencil_strokes then (
            Gl.enable t.impl Gl.stencil_test;
            stencil_mask t 0xff;

            (* Fill the stroke base without overlap *)
            stencil_func t Gl.equal 0x0 0xff;
            Gl.stencil_op t.impl Gl.keep Gl.keep Gl.incr;
            set_uniforms t call.uniforms2 call.image;
            check_error "stroke fill 0";
            DynArray.iter call.paths ~f:(fun path ->
                Gl.draw_arrays t.impl Gl.triangle_strip path.stroke_offset path.stroke_count;
            );

            (* Draw anti-aliased pixels *)
            set_uniforms t call.uniforms call.image;
            stencil_func t Gl.equal 0x0 0xff;
            Gl.stencil_op t.impl Gl.keep Gl.keep Gl.keep;
            DynArray.iter call.paths ~f:(fun path ->
                Gl.draw_arrays t.impl Gl.triangle_strip path.stroke_offset path.stroke_count;
            );

            (* Clear stencil buffer *)
            Gl.color_mask t.impl false false false false;
            stencil_func t Gl.always 0x0 0xff;
            Gl.stencil_op t.impl Gl.zero_ Gl.zero_ Gl.zero_;
            check_error "stroke fill 1";
            DynArray.iter call.paths ~f:(fun path ->
                Gl.draw_arrays t.impl Gl.triangle_strip path.stroke_offset path.stroke_count;
            );
            Gl.color_mask t.impl true true true true;

            Gl.disable t.impl Gl.stencil_test;
        ) else (
            set_uniforms t call.uniforms call.image;
            check_error "stroke fill";
            DynArray.iter call.paths ~f:(fun path ->
                Gl.draw_arrays t.impl Gl.triangle_strip path.stroke_offset path.stroke_count
            )
        );
        check_error "stroke end";
    ;;

    let fill t (call : Call.t) =
        (* Draw shapes *)
        Gl.enable t.impl Gl.stencil_test;
        stencil_mask t 0xff;
        stencil_func t Gl.always 0x0 0xff;
        Gl.color_mask t.impl false false false false;

        (* Set bindpoint for solid loc *)
        set_uniforms t call.uniforms 0;
        check_error "fill simple";

        Gl.stencil_op_separate t.impl Gl.front Gl.keep Gl.keep Gl.incr_wrap;
        Gl.stencil_op_separate t.impl Gl.back Gl.keep Gl.keep Gl.decr_wrap;
        Gl.disable t.impl Gl.cull_face_enum;
        DynArray.iter call.paths ~f:(fun path ->
            Gl.draw_arrays t.impl Gl.triangle_fan path.fill_offset path.fill_count;
        );
        Gl.enable t.impl Gl.cull_face_enum;

        (* Draw anti-aliased pixels *)
        Gl.color_mask t.impl true true true true;

        set_uniforms t call.uniforms2 call.image;
        check_error "fill fill";

        if CreateFlags.(has t.flags ~flag:antialias) then (
            stencil_func t Gl.equal 0x0 0xff;
            Gl.stencil_op t.impl Gl.keep Gl.keep Gl.keep;
            (* Draw fringes *)
            DynArray.iter call.paths ~f:(fun path ->
                Gl.draw_arrays t.impl Gl.triangle_strip path.stroke_offset path.stroke_count;
            );
        );

        (* Draw fill *)
        stencil_func t Gl.notequal 0x0 0xff;
        Gl.stencil_op t.impl Gl.zero_ Gl.zero_ Gl.zero_;
        Gl.draw_arrays t.impl Gl.triangle_strip call.triangle_offset call.triangle_count;

        Gl.disable t.impl Gl.stencil_test;
        check_error "fill end";
    ;;

    let convex_fill t (call : Call.t) =
        set_uniforms t call.uniforms call.image;
        check_error "convex fill";

        DynArray.iter call.paths ~f:(fun path ->
            Gl.draw_arrays t.impl Gl.triangle_fan path.fill_offset path.fill_count;
            (* Draw fringes *)
            if path.stroke_count > 0 then (
                Gl.draw_arrays t.impl Gl.triangle_strip path.stroke_offset path.stroke_count;
            )
        );
    ;;

    let triangles t (call : Call.t) =
        set_uniforms t call.uniforms call.image;
        Gl.draw_arrays t.impl Gl.triangles call.triangle_offset call.triangle_count;
        check_error "triangles fill";
    ;;

    let upload t verts =
        Gl.buffer_data 
            t.impl
            Gl.array_buffer 
            (VertexBuffer.unsafe_array verts)
            Gl.stream_draw;
    ;;

    let flush t verts =
        if FloatOps.(DynArray.length t.calls >. 0) then (
            (* Setup OpenGL state *)
            Gl.use_program t.impl t.shader;

            Gl.enable t.impl Gl.cull_face_enum;
            Gl.cull_face t.impl Gl.back;
            Gl.front_face t.impl Gl.ccw;
            Gl.enable t.impl Gl.blend;
            Gl.disable t.impl Gl.depth_test;
            Gl.disable t.impl Gl.scissor_test;
            Gl.color_mask t.impl true true true true;
            Gl.stencil_mask t.impl 0xffffffff;
            Gl.stencil_op t.impl Gl.keep Gl.keep Gl.keep;
            Gl.stencil_func t.impl Gl.always 0 0xffffffff;
            Gl.active_texture t.impl Gl.texture0;
            Gl.bind_texture t.impl Gl.texture_2d None;

            t.bound_texture <- None;
            t.stencil_mask <- 0xffffffff;
            t.stencil_func <- Gl.always;
            t.stencil_func_ref <- 0;
            t.stencil_func_mask <- 0xffffffff;

            t.blend_func <- Blend.empty;

            (* Upload vertex data *)
            Gl.bind_buffer t.impl Gl.array_buffer t.vert_buf;

            upload t verts;

            Gl.enable_vertex_attrib_array t.impl 0;
            Gl.enable_vertex_attrib_array t.impl 1;
            Gl.vertex_attrib_pointer t.impl 0 2 Gl.float false 16 0; (* Or data 0? *)
            Gl.vertex_attrib_pointer t.impl 1 2 Gl.float false 16 (0 + 2*4);

            (* TODO - don't bother using a hashtable, store them directly *)
            Gl.uniform1i t.impl t.locs.tex 0;
            Gl.uniform2fv t.impl t.locs.view_size t.view;

            DynArray.iter t.calls ~f:(fun call ->
                blend_func_separate t call.blend_func;
                match call.type_ with
                | Fill -> fill t call
                | Convex_fill -> convex_fill t call
                | Stroke -> stroke t call
                | Triangles -> triangles t call
            );

            Gl.disable_vertex_attrib_array t.impl 0;
            Gl.disable_vertex_attrib_array t.impl 1;
            Gl.disable t.impl Gl.cull_face_enum;
            (*Gl.bind_buffer Gl.array_buffer 0;
            Gl.use_program Gl.null_program;*)
            bind_texture t None;
        );

        (* Reset state *)
        cancel t
    ;;

    let render_triangles (t : t) (paint : Paint.t) composite_op scissor fringe verts =
        let call = DynArray.steal t.calls Call.empty in
        Call.reset call Call.Triangles;

        call.image <- paint.image;
        call.blend_func <- Blend.of_composite_op_state composite_op;

        (* Allocate vertices *)
        call.triangle_offset <- VertexBuffer.Sub.vertex_offset verts;
        call.triangle_count <- VertexBuffer.Sub.length verts;

        (* Fill shader *)
        convert_paint t call.uniforms paint scissor 1. fringe ~-.1.;
        FragUniforms.set_type call.uniforms ShaderType.img;

        check_error "render_triangles"
    ;;

    let triangles t ~paint ~composite_op ~scissor ~fringe ~vertices =
        render_triangles t paint composite_op scissor fringe vertices
    ;;

    let fill t ~paint ~composite_op ~scissor ~fringe ~bounds ~paths ~verts =
        render_fill t paint composite_op scissor fringe bounds paths verts
    ;;

    let stroke t ~paint ~composite_op ~scissor ~fringe ~stroke_width ~paths =
        render_stroke t paint composite_op scissor fringe stroke_width paths
    ;;
end
