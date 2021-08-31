open Graphv_core_lib

module Context = Context

module Make
    (Impl : Graphv_core_lib.Impl.S)
    (Font : Graphv_core_lib.Font_impl.S with type data := Impl.Buffer.UByte.t)
    : Context.S
        with type Buffer.UByte.t = Impl.Buffer.UByte.t
        and type Buffer.Float.t = Impl.Buffer.Float.t
        and type arg = Impl.gl
= struct

    type arg = Impl.gl

    module Buffer = Impl.Buffer
    module VertexBuffer = Impl.VertexBuffer
    module IPath = Impl.Path

    module Align = Graphv_core_lib.Align
    module BlendFactor = Graphv_core_lib.BlendFactor
    module Bounds = Graphv_core_lib.Bounds
    module Color = Graphv_core_lib.Color
    module CreateFlags = Graphv_core_lib.CreateFlags
    module CompositeOperation = Graphv_core_lib.CompositeOperation
    module CompositeOperationState = Graphv_core_lib.CompositeOperationState
    module ImageFlags = Graphv_core_lib.ImageFlags
    module LineCap = LineCap
    module LineJoin = LineJoin
    module Matrix = Graphv_core_lib.Matrix
    module Winding = Graphv_core_lib.Winding
    module FloatOps = Graphv_core_lib.FloatOps

    module FontContext = Font

    module PathCache = struct
        type t = {
            points : Point.t DynArray.t;
            paths : IPath.t DynArray.t;
            verts : VertexBuffer.t;
            mutable bounds : Bounds.t;
        }

        let create () = {
            (*points = DynArray.create 10 Point.(create ~x:0. ~y:0. ~flags:PointFlags.no_flags);*)
            points = DynArray.init 256 Point.(fun () -> create ~x:0. ~y:0. ~flags:PointFlags.no_flags);
            paths = DynArray.init 10 IPath.create;
            verts = VertexBuffer.create();
            bounds = Bounds.empty;
        }

        let clear t =
            DynArray.clear t.points;
            DynArray.clear t.paths;
    end

    type t = {
        impl : Impl.t;
        commands : Command.t DynArray.t;
        mutable command_x : float;
        mutable command_y : float;
        states : State.t DynArray.t;
        cache : PathCache.t;
        mutable tess_tol : float;
        mutable dist_tol : float;
        mutable fringe_width : float;
        mutable device_pixel_ratio : float;
        fs : FontContext.t;
        font_images : int DynArray.t;
        mutable font_image_idx : int;
        mutable draw_call_count : int;
        mutable fill_tri_count : int;
        mutable stroke_tri_count : int;
        mutable text_tri_count : int;
    }

    module Image = struct
        type image = int

        type data = Buffer.UByte.t

        let dummy : image = ~-1

        let from_buffer t ~(data : data) ~width ~height ~flags : image option =
            Impl.create_texture t.impl ~type_:`RGBA ~flags ~w:width ~h:height ~data
        ;;

        let from_color t ~data ~width ~height ~flags =
            (* Convert color array to byte array *)
            let len = Array.length data in
            assert (len = width*height);
            let img = Impl.Buffer.UByte.create (width*height*4) in
            for i=0 to len-1 do
                let color : Color.t = data.(i) in
                Impl.Buffer.UByte.set img (i*4+0) (int_of_float (color.r*.255.));
                Impl.Buffer.UByte.set img (i*4+1) (int_of_float (color.g*.255.));
                Impl.Buffer.UByte.set img (i*4+2) (int_of_float (color.b*.255.));
                Impl.Buffer.UByte.set img (i*4+3) (int_of_float (color.a*.255.));
            done;
            from_buffer t ~data:img ~flags ~width ~height
        ;;

        let update_image t ~image ~data =
            let w, h = Impl.get_texture_size t.impl ~image in
            Impl.update_texture t.impl ~image ~x:0 ~y:0 ~w ~h ~data
        ;;

        let size t image =
            Impl.get_texture_size t.impl ~image
        ;;

    end

    let opt_exn = function
        | None -> failwith "expected some"
        | Some t -> t

    let set_device_pixel_ratio t ratio =
        t.tess_tol <- 0.25 /. ratio;
        t.dist_tol <- 0.01 /. ratio;
        t.fringe_width <- 1. /. ratio;
        t.device_pixel_ratio <- ratio;
    ;;

    let get_state t =
        DynArray.last t.states
    ;;

    let save t =
        let state =
            if DynArray.empty t.states then (
                State.create()
            ) else (
                State.(copy (get_state t))
            )
        in
        DynArray.add t.states state
    ;;

    let restore t =
        DynArray.pop_back t.states;
    ;;

    let set_shape_antialias t ~enabled =
        let state = get_state t in
        state.shape_anti_alias <- enabled
    ;;

    let set_stroke_width t ~width =
        let state = get_state t in
        state.stroke_width <- width
    ;;

    let set_miter_limit t ~limit =
        let state = get_state t in
        state.miter_limit <- limit
    ;;

    let set_line_cap t ~cap =
        let state = get_state t in
        state.line_cap <- cap
    ;;

    let set_line_join t ~join =
        let state = get_state t in
        state.line_join <- join
    ;;

    module Transform = struct
        let transform t m =
            let state = get_state t in
            Matrix.premultiply ~dst:state.xform ~src:m
        ;;

        let reset t =
            let state = get_state t in
            Matrix.identity state.xform
        ;;

        let translate t ~x ~y =
            let state = get_state t in
            let m = Matrix.create() in
            Matrix.translate m ~x ~y;
            Matrix.premultiply ~dst:state.xform ~src:m;
        ;;

        let rotate t ~angle =
            let state = get_state t in
            let m = Matrix.create() in
            Matrix.rotate m ~angle;
            Matrix.premultiply ~dst:state.xform ~src:m;
        ;;

        let skew_x t ~angle =
            let state = get_state t in
            let m = Matrix.create() in
            Matrix.skew_x m ~angle;
            Matrix.premultiply ~dst:state.xform ~src:m
        ;;

        let skew_y t ~angle =
            let state = get_state t in
            let m = Matrix.create() in
            Matrix.skew_y m ~angle;
            Matrix.premultiply ~dst:state.xform ~src:m
        ;;

        let scale t ~x ~y =
            let state = get_state t in
            let m = Matrix.create() in
            Matrix.scale m ~xs:x ~ys:y;
            Matrix.premultiply ~dst:state.xform ~src:m
        ;;

        let [@inline always] deg_to_rad d = Float.(d *. pi /. 180.)
        let [@inline always] rad_to_deg r = Float.(r *. 180. /. pi)

        let current_transform t =
            (get_state t).xform
        ;;
    end

    module Scissor = struct
        let scissor t ~x ~y ~w ~h =
            let state = get_state t in

            let w = Float.max w 0. in
            let h = Float.max h 0. in

            Matrix.identity state.scissor.xform;
            let m = state.scissor.xform in
            m.m4 <- x+.w*.0.5;
            m.m5 <- y+.h*.0.5;
            Matrix.multiply ~dst:state.scissor.xform ~src:state.xform;

            state.scissor <- Scissor.{
                state.scissor with
                extent_0 = w*.0.5;
                extent_1 = h*.0.5;
            };
        ;;

        let intersect_rects ax ay aw ah bx by bw bh =
            let minx = Float.max ax bx in
            let miny = Float.max ay by in
            let maxx = Float.min (ax+.aw) (bx+.bw) in
            let maxy = Float.min (ay+.ah) (by+.bh) in
            minx, miny, Float.(max 0. (maxx-.minx)), Float.(max 0. (maxy -. miny))
        ;;

        let intersect t ~x ~y ~w ~h =
            let state = get_state t in

            if state.scissor.extent_0 < 0. then (
                scissor t ~x ~y ~w ~h;
            ) else (
                let pxform = Matrix.copy state.scissor.xform in
                let invxform = Matrix.create() in

                let ex = state.scissor.extent_0
                and ey = state.scissor.extent_1 in

                Matrix.inverse ~dst:invxform ~src:state.xform;
                Matrix.multiply ~dst:pxform ~src:invxform;

                let p = pxform in
                let tex = ex*.(Float.abs p.m0) +. ey*.(Float.abs p.m2) in
                let tey = ex*.(Float.abs p.m1) +. ey*.(Float.abs p.m3) in

                let x, y, w, h = intersect_rects
                    (p.m4 -. tex)
                    (p.m5 -. tey)
                    (tex*.2.)
                    (tey*.2.)
                    x y w h
                in

                scissor t ~x ~y ~w ~h
            )
        ;;

        let reset t =
            let state = get_state t in
            state.scissor <- Scissor.create()
        ;;
    end

    module Global = struct
        let set_composite_operation t ~op =
            let state = get_state t in
            state.composite_operation <- op
        ;;

        let set_composite_blend_func_separate
            t
            ~src_rgb
            ~dst_rgb
            ~src_alpha
            ~dst_alpha
            =
            let op = CompositeOperationState.{
                src_rgb;
                dst_rgb;
                src_alpha;
                dst_alpha;
            } in
            let state = get_state t in
            state.composite_operation <- op
        ;;

        let set_composite_blend_func t ~src ~dst =
            set_composite_blend_func_separate t
                ~src_rgb:src ~dst_rgb:dst
                ~src_alpha:src ~dst_alpha:dst
        ;;

        let set_alpha t ~alpha =
            let state = get_state t in
            state.alpha <- alpha
        ;;
    end

    let last_path t = DynArray.last t.cache.paths

    let add_path t =
        let npoints = DynArray.length t.cache.points in
        let path = DynArray.steal t.cache.paths IPath.create in
        IPath.reset path;

        path.first <- npoints;
    ;;

    let close_path t =
        let path = last_path t in
        path.closed <- true;
    ;;

    let path_winding t winding =
        let path = last_path t in
        path.winding <- winding;
    ;;

    let last_point t = DynArray.last t.cache.points

    let add_point t x y flags =
        let path = last_path t in

        let insert () =
            let point = DynArray.steal t.cache.points Point.empty in
            Point.reset point x y flags;
            path.count <- path.count + 1;
        in

        if path.count > 0 && DynArray.length t.cache.points > 0 then (
            let last = last_point t in
            if Point.equals last.x last.y x y t.dist_tol then (
                last.flags <- PointFlags.add last.flags ~flag:flags;
            ) else (
                insert()
            )
        ) else (
            insert()
        )
    ;;

    let [@inline always] fabs (a : float) : float =
        if a < 0. then ~-.a else a

    let rec tesselate_bezier t x1 y1 x2 y2 x3 y3 x4 y4 level typ =
        let open FloatOps in
        if level <. 10 then (
            let dx = x4 - x1 in
            let dy = y4 - y1 in
            let d2 = fabs ((x2 - x4)*dy - (y2 - y4)*dx) in
            let d3 = fabs ((x3 - x4)*dy - (y3 - y4)*dx) in

            if (d2+d3)*(d2+d3) < t.tess_tol * (dx*dx + dy*dy) then (
                add_point t x4 y4 typ
            ) else (
                let x23 = (x2+x3)*0.5 in
                let x34 = (x3+x4)*0.5 in
                let x234 = (x23 + x34)*0.5 in

                let x12 = (x1+x2)*0.5 in
                let x123 = (x12+x23)*0.5 in
                let x1234 = (x123 + x234)*0.5 in

                let y23 = (y2+y3)*0.5 in
                let y34 = (y3+y4)*0.5 in
                let y234 = (y23 + y34)*0.5 in

                let y12 = (y1+y2)*0.5 in
                let y123 = (y12+y23)*0.5 in
                let y1234 = (y123 + y234)*0.5 in

                tesselate_bezier t x1 y1 x12 y12 x123 y123 x1234 y1234 (level +. 1) PointFlags.no_flags;
                tesselate_bezier t x1234 y1234 x234 y234 x34 y34 x4 y4 (level +. 1) typ;
            )
        )
    ;;

    let triarea2 ax ay bx by cx cy =
        let open FloatOps in
        let abx = bx - ax in
        let aby = by - ay in
        let acx = cx - ax in
        let acy = cy - ay in
        acx*aby - abx*acy
    ;;

    let poly_area (points : Point.t array) offset count =
        let area = ref 0. in
        let a = points.(offset) in
        for i=2 to count-1 do
            let b = points.(offset + i - 1) in
            let c = points.(offset + i) in
            area := !area +. triarea2 a.x a.y b.x b.y c.x c.y
        done;
        !area *. 0.5
    ;;

    let poly_reverse (points : Point.t array) offset count =
        let i = ref 0 in
        let j = ref (count - 1) in
        while !i < !j do
            let tmp = points.(offset + !i) in
            points.(offset + !i) <- points.(offset + !j);
            points.(offset + !j) <- tmp;
            incr i;
            decr j;
        done
    ;;

    module Path = struct
        let begin_ t =
            DynArray.clear t.commands;
            PathCache.clear t.cache;
        ;;

        let transform_command t (cmd : Command.t) : Command.t =
            let open FloatOps in
            let xform = (get_state t).xform in
            match cmd with
            | Move_to {x;y} ->
                (*let x, y = Matrix.transform_point xform x y in*)
                let xn = x*xform.m0 + y*xform.m2 + xform.m4 in
                let yn = x*xform.m1 + y*xform.m3 + xform.m5 in
                Move_to {x=xn;y=yn}
            | Line_to {x;y} ->
                (*let x, y = Matrix.transform_point xform x y in*)
                let xn = x*xform.m0 + y*xform.m2 + xform.m4 in
                let yn = x*xform.m1 + y*xform.m3 + xform.m5 in
                Line_to {x=xn;y=yn}
            | Bezier_to {c1x;c1y;c2x;c2y;x;y} ->
                (*let x, y = Matrix.transform_point xform x y in
                let c1x, c1y = Matrix.transform_point xform c1x c1y in
                let c2x, c2y = Matrix.transform_point xform c2x c2y in
                *)
                let xn = x*xform.m0 + y*xform.m2 + xform.m4 in
                let yn = x*xform.m1 + y*xform.m3 + xform.m5 in
                let c1xn = c1x*xform.m0 + c1y*xform.m2 + xform.m4 in
                let c1yn = c1x*xform.m1 + c1y*xform.m3 + xform.m5 in
                let c2xn = c2x*xform.m0 + c2y*xform.m2 + xform.m4 in
                let c2yn = c2x*xform.m1 + c2y*xform.m3 + xform.m5 in
                Bezier_to {c1x=c1xn;c1y=c1yn;c2x=c2xn;c2y=c2yn;x=xn;y=yn}
            | Close as c -> c
            | Winding _ as w -> w
        ;;

        let add_command t cmd =
            DynArray.add t.commands (transform_command t cmd)
        ;;

        let close t = add_command t Command.Close
        let winding t ~winding = add_command t Command.(Winding winding)
        let move_to t ~x ~y = add_command t Command.(Move_to {x;y})
        let line_to t ~x ~y = add_command t Command.(Line_to {x;y})
        let bezier_to t ~c1x ~c1y ~c2x ~c2y ~x ~y =
            add_command t Command.(Bezier_to {c1x; c1y; c2x; c2y; x; y})
        let quad_to t ~cx ~cy ~x ~y =
            let open FloatOps in
            let x0 = t.command_x in
            let y0 = t.command_y in
            let bezier = Command.Bezier_to {
                c1x = x0 + 2./3.*(cx - x0);
                c1y = y0 + 2./3.*(cy - y0);
                c2x = x + 2./3.*(cx - x);
                c2y = y + 2./3.*(cy - y);
                x; y;
            } in
            add_command t bezier
        ;;

        let rect t ~x ~y ~w ~h =
            let open FloatOps in
            add_command t Command.(Move_to {x; y});
            add_command t Command.(Line_to {x; y=y+h});
            add_command t Command.(Line_to {x=x+w; y=y+h});
            add_command t Command.(Line_to {x=x+w; y});
            add_command t Command.Close
        ;;

        let cross dx0 dy0 dx1 dy1 =
            dx1*.dy0 +. dx0*.dy1
        ;;

        let normalize_arc_angle da (dir : Winding.t) =
            let open FloatOps in
            let _2_pi = Float.pi*2. in
            match dir with
            | CW ->
                if Float.abs da >= _2_pi then (
                    _2_pi
                ) else (
                    let da = ref da in
                    while !da < 0. do
                        da := !da + _2_pi
                    done;
                    !da
                )
            | CCW ->
                if Float.abs da >= _2_pi then (
                    ~-._2_pi
                ) else (
                    let da = ref da in
                    while !da > 0. do
                        da := !da - _2_pi
                    done;
                    !da
                )
        ;;

        let arc t ~cx ~cy ~r ~a0 ~a1 ~dir =
            let open FloatOps in
            let use_line_to = DynArray.length t.commands >. 0 in

            let da = normalize_arc_angle (a1 - a0) dir in
            let min_1 = Float.abs da / Float.(pi*0.5) + 0.5 |> Int.of_float in
            let ndivs = imax 1 (imin min_1 5) in
            let hda = (da / float ndivs) / 2. in
            let kappa = Float.abs (4. / 3. * (1. - Float.cos hda) / Float.sin hda) in
            let kappa = if Stdlib.(dir = Winding.CCW) then ~-.kappa else kappa in

            let ptanx = ref 0. in
            let ptany = ref 0. in
            let px = ref 0. in
            let py = ref 0. in

            for i=0 to ndivs do
                let a = a0 + da*(float i / float ndivs) in
                let dx = Float.cos a in
                let dy = Float.sin a in
                let x = cx + dx*r in
                let y = cy + dy*r in
                let tanx = ~-.dy*r*kappa in
                let tany = dx*r*kappa in

                if i =. 0 then (
                    if use_line_to then (
                        add_command t (Command.Line_to {x;y})
                    ) else (
                        add_command t (Command.Move_to {x;y})
                    )
                ) else (
                    add_command t (Command.Bezier_to {
                        c1x = !px + !ptanx;
                        c1y = !py + !ptany;
                        c2x = x - tanx;
                        c2y = y - tany;
                        x;y;
                    })
                );

                px := x;
                py := y;
                ptanx := tanx;
                ptany := tany;
            done
        ;;

        let arc_to (t : t) ~x1 ~y1 ~x2 ~y2 ~radius =
            let open FloatOps in
            let x0 = t.command_x in
            let y0 = t.command_y in

            (* Handle degenerate cases *)
            if Point.equals x0 y0 x1 y1 t.dist_tol
               || Point.equals x1 y1 x2 y2 t.dist_tol
               || Point.dist_segment x1 y1 x0 y0 x2 y2 < t.dist_tol*t.dist_tol
               || radius < t.dist_tol
            then (
                line_to t ~x:x1 ~y:y1
            ) else (
                let dx0 = x0 - x1 in
                let dy0 = y0 - y1 in
                let dx1 = x2 - x1 in
                let dy1 = y2 - y1 in
                let _, dx0, dy0 = Point.normalize dx0 dy0 in
                let _, dx1, dy1 = Point.normalize dx1 dy1 in
                let a = Float.acos (dx0*dx1 + dy0*dy1) in
                let d = radius / Float.tan (a/2.0) in

                if d > 10000. then (
                    line_to t ~x:x1 ~y:y1
                ) else if cross dx0 dy0 dx1 dy1 > 0. then (
                    let cx = x1 + dx0*d + dy0*radius in
                    let cy = y1 + dy0*d + (~-.dx0)*radius in
                    let a0 = Float.atan2 dx0 ~-.dy0 in
                    let a1 = Float.atan2 ~-.dx1 dy1 in
                    let dir = Winding.CW in
                    arc t ~cx ~cy ~r:radius ~a0 ~a1 ~dir
                ) else (
                    let cx = x1 + dx0*d + ~-.dy0*radius in
                    let cy = y1 + dy0*d + dx0*radius in
                    let a0 = Float.atan2 ~-.dx0 dy0 in
                    let a1 = Float.atan2 dx1 ~-.dy1 in
                    let dir = Winding.CCW in
                    arc t ~cx ~cy ~r:radius ~a0 ~a1 ~dir
                )
            )
        ;;

        let kappa_90 = 0.5522847493

        let ellipse t ~cx ~cy ~rx ~ry =
            let open FloatOps in
            let open Command in
            let bz_to c1x c1y c2x c2y x y = add_command t (Bezier_to {c1x; c1y; c2x; c2y; x; y}) in
            add_command t (Move_to {x=cx-rx; y=cy});
            bz_to (cx-rx) (cy+ry*kappa_90) (cx-rx*kappa_90) (cy+ry) cx (cy+ry);
            bz_to (cx+rx*kappa_90) (cy+ry) (cx+rx) (cy+ry*kappa_90) (cx+rx) cy;
            bz_to (cx+rx) (cy-ry*kappa_90) (cx+rx*kappa_90) (cy-ry) cx (cy-ry);
            bz_to (cx-rx*kappa_90) (cy-ry) (cx-rx) (cy-ry*kappa_90) (cx-rx) cy;
            add_command t Close
        ;;

        let circle t ~cx ~cy ~r =
            ellipse t ~cx ~cy ~rx:r ~ry:r
        ;;

        let sign a = if a >= 0. then 1. else ~-.1.

        let rounded_rect_varying t ~x ~y ~w ~h ~top_left ~top_right ~bot_left ~bot_right =
            if top_left < 0.1 && top_right < 0.1 && bot_left < 0.1 && bot_right < 0.1 then (
                rect t ~x ~y ~w ~h
            ) else (
                let open FloatOps in
                let open Command in
                let bz_to c1x c1y c2x c2y x y = add_command t (Bezier_to {c1x; c1y; c2x; c2y; x; y}) in
                let line_to x y = add_command t (Line_to {x;y}) in
                let halfw = Float.abs w * 0.5 in
                let halfh = Float.abs h * 0.5 in
                let rxBL = Float.(min bot_left halfw) * sign w in
                let ryBL = Float.(min bot_left halfh) * sign h in
                let rxBR = Float.(min bot_right halfw) * sign w in
                let ryBR = Float.(min bot_right halfh) * sign h in
                let rxTR = Float.(min top_right halfw) * sign w in
                let ryTR = Float.(min top_right halfh) * sign h in
                let rxTL = Float.(min top_left halfw) * sign w in
                let ryTL = Float.(min top_left halfh) * sign h in

                add_command t (Move_to {x; y=y+ryTL});
                line_to x (y+h-ryBL);
                bz_to x (y+h-ryBL*(1.-kappa_90)) (x + rxBL*(1.-kappa_90)) (y+h) (x+rxBL) (y+h);
                line_to (x+w-rxBR) (y+h);
                bz_to (x+w-rxBR*(1.-kappa_90)) (y+h) (x+w) (y+h-ryBR*(1.-kappa_90)) (x+w) (y+h-ryBR);
                line_to (x+w) (y+ryTR);
                bz_to (x+w) (y+ryTR*(1.-kappa_90)) (x+w-rxTR*(1.-kappa_90)) y (x+w-rxTR) y;
                line_to (x+rxTL) y;
                bz_to (x+rxTL*(1.-kappa_90)) y x (y+ryTL*(1.-kappa_90)) x (y+ryTL);
                add_command t Close
            )
        ;;

        let rounded_rect t ~x ~y ~w ~h ~r =
            rounded_rect_varying t ~x ~y ~w ~h ~top_left:r ~top_right:r ~bot_left:r ~bot_right:r
        ;;

        let normalize_pt (pt : Point.t)=
            let open Graphv_core_lib.FloatOps in
            let d = Float.sqrt (pt.dx*pt.dx + pt.dy*pt.dy) in
            pt.len <- d;
            if d > 1e-6 then (
                let id = 1. / d in
                pt.dx <- pt.dx*id;
                pt.dy <- pt.dy*id;
            );
        ;;

        let flatten t =
            if DynArray.length t.cache.paths = 0 then (
                DynArray.iter t.commands ~f:(function
                    | Move_to {x;y} ->
                        add_path t;
                        add_point t x y PointFlags.corner;
                    | Line_to {x;y} ->
                        add_point t x y PointFlags.corner;
                    | Bezier_to {c1x; c1y; c2x; c2y; x; y} ->
                        let last = last_point t in
                        tesselate_bezier t last.x last.y c1x c1y c2x c2y x y 0 PointFlags.corner
                    | Close -> close_path t
                    | Winding w -> path_winding t w
                );

                let xmin = ref 1e6 in
                let ymin = ref 1e6 in
                let xmax = ref ~-.1e6 in
                let ymax = ref ~-.1e6 in

                let points = t.cache.points in
                (* Wish we had sub arrays for DynArray *)
                DynArray.iter t.cache.paths ~f:(fun path ->
                    let get i =
                        DynArray.get points (path.first + i)
                    in
                    (* If first and last points are the same, remove last, mark as closed *)
                    let p0 = get (path.count - 1) |> ref in
                    let p1 = get 0 in
                    if Point.equals !p0.x !p0.y p1.x p1.y t.dist_tol then (
                        path.count <- path.count - 1;
                        p0 := get (path.count - 1);
                        path.closed <- true;
                    );

                    (* Enforce winding *)
                    let pts = DynArray.unsafe_array points in
                    if path.count > 2 then (
                        let area = poly_area pts path.first path.count in
                        if path.winding = Winding.CCW && area < 0. then (
                            poly_reverse pts path.first path.count
                        );

                        if path.winding = Winding.CW && area > 0. then (
                            poly_reverse pts path.first path.count
                        );
                    );

                    let p1_off = ref 0 in
                    p0 := get (path.count - 1);
                    for _=0 to path.count-1 do
                        let open FloatOps in
                        (* Calc segment directions *)
                        (* This calc is good *)
                        let p1 = get !p1_off in
                        !p0.dx <- p1.x - !p0.x;
                        !p0.dy <- p1.y - !p0.y;

                        normalize_pt !p0;

                        (* Update bounds *)
                        let open FloatOps in
                        xmin := min !xmin !p0.x;
                        ymin := min !ymin !p0.y;
                        xmax := max !xmax !p0.x;
                        ymax := max !ymax !p0.y;

                        p0 := get !p1_off;
                        incr p1_off;
                    done;
                );

                t.cache.bounds <- Bounds.{
                    xmin = !xmin;
                    ymin = !ymin;
                    xmax = !xmax;
                    ymax = !ymax;
                }
            )
        ;;
    end

    let reset t =
        let state = get_state t in
        State.reset state;

        Paint.set_color state.fill Color.white;
        Paint.set_color state.stroke Color.black;
        state.composite_operation <- CompositeOperationState.of_composite_operation CompositeOperation.Source_over;
        state.shape_anti_alias <- true;
        state.stroke_width <- 1.;
        state.miter_limit <- 10.;
        state.line_cap <- LineCap.Butt;
        state.line_join <- LineJoin.Miter;
        state.alpha <- 1.;
        Matrix.identity state.xform;

        state.scissor <- {
            xform = Matrix.create();
            extent_0 = ~-.1.;
            extent_1 = ~-.1.;
        };

        state.font_size <- 16.;
        state.letter_spacing <- 0.;
        state.line_height <- 1.;
        state.font_blur <- 0.;
        state.text_align <- (*Align.or_ Align.left Align.baseline*) Align.left;
        state.font_id <- 0;
    ;;

    let begin_frame t ~width ~height ~device_ratio =
        DynArray.clear t.states;
        save t;
        reset t;
        set_device_pixel_ratio t device_ratio;
        Impl.viewport t.impl ~width ~height ~dpi:device_ratio;

        t.draw_call_count <- 0;
        t.fill_tri_count <- 0;
        t.stroke_tri_count <- 0;
        t.text_tri_count <- 0;
        VertexBuffer.clear t.cache.verts;
    ;;

    let cancel_frame t =
        Impl.cancel t.impl
    ;;

    let calculate_joins t w line_join miter_limit =
        let iw =
            if w > 0. then (
                1.0 /. w
            ) else (
                0.
            )
        in

        DynArray.iter t.cache.paths ~f:(fun path ->
            let [@inline always] get idx =
                DynArray.get t.cache.points (path.first + idx)
            in
            let p0_off = ref (path.count - 1) in
            let p1_off = ref 0 in
            let left = ref 0 in

            path.nbevel <- 0;

            for _=0 to path.count-1 do
                let p0 = get !p0_off in
                let p1 = get !p1_off in

                let dlx0 = p0.dy in
                let dly0 = ~-.(p0.dx) in
                let dlx1 = p1.dy in
                let dly1 = ~-.(p1.dx) in

                (* Calculate extrusions *)
                (* these are right *)
                p1.dmx <- (dlx0 +. dlx1) *. 0.5;
                p1.dmy <- (dly0 +. dly1) *. 0.5;

                let dmr2 = p1.dmx*.p1.dmx +. p1.dmy*.p1.dmy in
                if dmr2 > 0.000001 then (
                    let s = 1. /. dmr2 in
                    let scale = if s > 600. then 600. else s in
                    p1.dmx <- p1.dmx *. scale;
                    p1.dmy <- p1.dmy *. scale;
                );

                (* Clear flags, keep corner *)
                p1.flags <-
                    if PointFlags.has p1.flags ~flag:PointFlags.corner
                    then PointFlags.corner
                    else PointFlags.no_flags
                ;

                (* Keep track of left turns *)
                let cross = p1.dx*.p0.dy -. p0.dx*.p1.dy in
                if cross > 0. then (
                    incr left;
                    p1.flags <- PointFlags.add p1.flags ~flag:PointFlags.left
                );

                (* Calculate if we should use bevel or miter for inner join *)
                let limit = Float.max 1.01 ((Float.min p0.len p1.len) *. iw) in
                if (dmr2 *. limit*.limit) < 1. then (
                    p1.flags <- PointFlags.add p1.flags ~flag:PointFlags.inner_bevel
                );

                (* Check to see if the corner needs to be beveled *)
                if PointFlags.has p1.flags ~flag:PointFlags.corner then (
                    if dmr2*.miter_limit*.miter_limit < 1.
                        || line_join = LineJoin.Bevel
                        || line_join = LineJoin.Round
                    then (
                        p1.flags <- PointFlags.add p1.flags ~flag:PointFlags.bevel
                    )
                );

                if PointFlags.has p1.flags ~flag:PointFlags.bevel
                    || PointFlags.has p1.flags ~flag:PointFlags.inner_bevel
                then (
                    path.nbevel <- path.nbevel + 1;
                );

                p0_off := !p1_off;
                incr p1_off;
            done;

            path.convex <- !left = path.count;
        )
    ;;

    let choose_bevel bevel (p0 : Point.t) (p1 : Point.t) w =
        if bevel then (
            let x0 = p1.x +. p0.dy*.w in
            let y0 = p1.y -. p0.dx*.w in
            let x1 = p1.x +. p1.dy*.w in
            let y1 = p1.y -. p1.dx*.w in
            x0, y0, x1, y1
        ) else (
            let x0 = p1.x +. p1.dmx*.w in
            let y0 = p1.y +. p1.dmy*.w in
            let x1 = p1.x +. p1.dmx*.w in
            let y1 = p1.y +. p1.dmy*.w in
            x0, y0, x1, y1
        )
    ;;

    let bevel_join verts offset (p0 : Point.t) (p1 : Point.t) lw rw lu ru =
        let offset = ref offset in
        let [@inline always] set x y u v =
            VertexBuffer.set verts !offset x y u v;
            incr offset;
        in
        let dlx0 = p0.dy in
        let dly0 = ~-.(p0.dx) in
        let dlx1 = p1.dy in
        let dly1 = ~-.(p1.dx) in
        let inner = PointFlags.has p1.flags ~flag:PointFlags.inner_bevel in
        if PointFlags.has p1.flags ~flag:PointFlags.left then (
            let lx0, ly0, lx1, ly1 = choose_bevel inner p0 p1 lw in
            set lx0 ly0 lu 1.;
            set (p1.x -. dlx0*.rw) (p1.y -. dly0*.rw) ru 1.;

            if PointFlags.has p1.flags ~flag:PointFlags.bevel then (
                set lx0 ly0 lu 1.;
                set (p1.x -. dlx0*.rw) (p1.y -. dly0*.rw) ru 1.;

                set lx1 ly1 lu 1.;
                set (p1.x -. dlx1*.rw) (p1.y -. dly1*.rw) ru 1.;
            ) else (
                let rx0 = p1.x -. p1.dmx*.rw in
                let ry0 = p1.y -. p1.dmy*.rw in

                set p1.x p1.y 0.5 1.;
                set (p1.x -. dlx0*.rw) (p1.y -. dly0*.rw) ru 1.;

                set rx0 ry0 ru 1.;
                set rx0 ry0 ru 1.;

                set p1.x p1.y 0.5 1.;
                set (p1.x -. dlx1*.rw) (p1.y -. dly1*.rw) ru 1.;
            );

            set lx1 ly1 lu 1.;
            set (p1.x -. dlx1*.rw) (p1.y -. dly1*.rw) ru 1.;
        ) else (
            let rx0, ry0, rx1, ry1 = choose_bevel inner p0 p1 ~-.rw in

            set (p1.x +. dlx0*.lw) (p1.y +. dly0*.lw) lu 1.;
            set rx0 ry0 ru 1.;

            if PointFlags.has p1.flags ~flag:PointFlags.bevel then (
                set (p1.x +. dlx0*.lw) (p1.y +. dly0*.lw) lu 1.;
                set rx0 ry0 ru 1.;

                set (p1.x +. dlx1*.lw) (p1.y +. dly1*.lw) lu 1.;
                set rx1 ry1 ru 1.;
            ) else (
                let lx0 = p1.x +. p1.dmx*.lw in
                let ly0 = p1.y +. p1.dmy*.lw in

                set (p1.x +. dlx0*.lw) (p1.y +. dly0*.lw) lu 1.;
                set p1.x p1.y 0.5 1.;

                set lx0 ly0 lu 1.;
                set lx0 ly0 lu 1.;

                set (p1.x +. dlx1*.lw) (p1.y +. dly1*.lw) lu 1.;
                set p1.x p1.y 0.5 1.;
            );

            set (p1.x +. dlx1*.lw) (p1.y +. dly1*.lw) lu 1.;
            set rx1 ry1 ru 1.;
        );
        !offset
    ;;

    let expand_fill (t : t) w line_join miter_limit =
        let open FloatOps in
        let aa = t.fringe_width in
        let fringe = w > 0. in

        calculate_joins t w line_join miter_limit;

        let convex =
            DynArray.length t.cache.paths =. 1
            && DynArray.(first t.cache.paths).convex
        in

        let woff = 0.5*aa in
        let verts = ref VertexBuffer.(num_verts t.cache.verts) in
        let dst = ref !verts in
        DynArray.iter t.cache.paths ~f:(fun path ->
            dst := !verts;
            let get_pt idx =
                DynArray.get t.cache.points (path.first +. idx)
            in
            if fringe then (
                let p0 = get_pt (path.count-.1) |> ref in
                let p1_off = ref 0 in

                for _=0 to path.count-.1 do
                    let p1 = get_pt (!p1_off) in
                    if PointFlags.has p1.flags ~flag:PointFlags.bevel then (
                        if PointFlags.has p1.flags ~flag:PointFlags.left then (
                            let lx = p1.x + p1.dmx*woff in
                            let ly = p1.y + p1.dmy*woff in
                            VertexBuffer.set t.cache.verts !dst lx ly 0.5 1.;
                            incr dst;
                        ) else (
                            let dlx0 = !p0.dy in
                            let dly0 = ~-.(!p0.dy) in
                            let dlx1 = p1.dy in
                            let dly1 = ~-.(p1.dx) in
                            let lx0 = p1.x + dlx0*woff in
                            let ly0 = p1.y + dly0*woff in
                            let lx1 = p1.x + dlx1*woff in
                            let ly1 = p1.y + dly1*woff in
                            VertexBuffer.set t.cache.verts !dst lx0 ly0 0.5 1.;
                            incr dst;
                            VertexBuffer.set t.cache.verts !dst lx1 ly1 0.5 1.;
                            incr dst;
                        )
                    ) else (
                        (* these are right *)
                        let x = p1.x + p1.dmx*woff in
                        let y = p1.y + p1.dmy*woff in
                        VertexBuffer.set t.cache.verts !dst x y 0.5 1.;
                        incr dst;
                    );
                    p0 := p1;
                    incr p1_off;
                done
            ) else (
                for j=0 to path.count-.1 do
                    let point = get_pt j in
                    VertexBuffer.set t.cache.verts !dst point.x point.y 0.5 1.;
                    incr dst;
                done
            );

            let nfill = (!dst -. !verts) in
            path.fill <- VertexBuffer.Sub.sub t.cache.verts !verts nfill;
            verts := !dst;

            let flag =
                PointFlags.no_flags
                |> PointFlags.add ~flag:PointFlags.bevel
                |> PointFlags.add ~flag:PointFlags.inner_bevel
            in

            if fringe then (
                let lw, lu =
                    if convex then (
                        woff, 0.5
                    ) else (
                        w + woff, 0.
                    )
                in
                let rw = w - woff in
                let ru = 1. in
                dst := !verts;

                let p1_off = ref 0 in
                let p0_off = ref (path.count -. 1) in
                for _=0 to path.count-.1 do
                    let p0 = get_pt !p0_off in
                    let p1 = get_pt !p1_off in
                    if PointFlags.has p1.flags ~flag then (
                        dst := bevel_join t.cache.verts !dst p0 p1 lw rw lu ru
                    ) else (
                        let x = p1.x + p1.dmx*lw in
                        let y = p1.y + p1.dmy*lw in
                        VertexBuffer.set t.cache.verts !dst x y lu 1.;
                        incr dst;
                        let x = p1.x - p1.dmx*rw in
                        let y = p1.y - p1.dmy*rw in
                        VertexBuffer.set t.cache.verts !dst x y ru 1.;
                        incr dst;
                    );
                    p0_off := !p1_off;
                    incr p1_off;
                done;

                (* Loop it *)
                let v0_x, v0_y, _, _ = VertexBuffer.get t.cache.verts !verts in
                let v1_x, v1_y, _, _ = VertexBuffer.get t.cache.verts (!verts +. 1) in
                VertexBuffer.set t.cache.verts !dst v0_x v0_y lu 1.;
                incr dst;
                VertexBuffer.set t.cache.verts !dst v1_x v1_y ru 1.;
                incr dst;

                let nstroke = !dst -. !verts in
                assert (nstroke >. 0);
                path.stroke <- VertexBuffer.Sub.sub t.cache.verts !verts nstroke;

                verts := !dst
            ) else (
                path.stroke <- VertexBuffer.Sub.empty;
            );
        );
    ;;

    let curve_divs r arc tol =
        let open FloatOps in
        let da = (Float.acos (r / (r + tol))) * 2. in
        imax 2 (int_of_float (Float.ceil (arc / da)))
    ;;

    let butt_cap_start verts dst (p : Point.t) dx dy w d aa u0 u1 =
        let after = dst + 4 in
        let open FloatOps in
        let px = p.x - dx*d in
        let py = p.y - dy*d in
        let dlx = dy in
        let dly = ~-.dx in
        VertexBuffer.set verts (dst+.0) (px + dlx*w - dx*aa) (py + dly*w - dy*aa) u0 0.;
        VertexBuffer.set verts (dst+.1) (px - dlx*w - dx*aa) (py - dly*w - dy*aa) u1 0.;
        VertexBuffer.set verts (dst+.2) (px + dlx*w) (py + dly*w) u0 1.;
        VertexBuffer.set verts (dst+.3) (px - dlx*w) (py - dly*w) u1 1.;
        after
    ;;

    let butt_cap_end verts dst (p : Point.t) dx dy w d aa u0 u1 =
        let after = dst + 4 in
        let open FloatOps in
        let px = p.x + dx*d in
        let py = p.y + dy*d in
        let dlx = dy in
        let dly = ~-.dx in
        VertexBuffer.set verts (dst+.0) (px + dlx*w) (py + dly*w) u0 1.;
        VertexBuffer.set verts (dst+.1) (px - dlx*w) (py - dly*w) u1 1.;
        VertexBuffer.set verts (dst+.2) (px + dlx*w + dx*aa) (py + dly*w + dy*aa) u0 0.;
        VertexBuffer.set verts (dst+.3) (px - dlx*w + dx*aa) (py - dly*w + dy*aa) u1 0.;
        after
    ;;

    let round_cap_start verts dst (p : Point.t) dx dy w ncap u0 u1 =
        let open FloatOps in
        let px = p.x in
        let py = p.y in
        let dlx = dy in
        let dly = ~-.dx in
        let i = ref 0 in
        while !i <. ncap do
            let a = float !i / float (ncap -. 1) * Float.pi in
            let ax = Float.cos a * w in
            let ay = Float.sin a * w in
            VertexBuffer.set verts !dst (px - dlx*ax - dx*ay) (py - dly*ax - dy*ay) u0 1.;
            incr dst;
            VertexBuffer.set verts !dst px py 0.5 1.;
            incr dst;
            incr i;
        done;
        VertexBuffer.set verts !dst (px + dlx*w) (py + dly*w) u0 1.;
        incr dst;
        VertexBuffer.set verts !dst (px - dlx*w) (py - dly*w) u1 1.;
        incr dst;
    ;;

    let round_cap_end verts dst (p : Point.t) dx dy w ncap u0 u1 =
        let i = ref 0 in
        let open FloatOps in
        let px = p.x in
        let py = p.y in
        let dlx = dy in
        let dly = ~-.dx in
        VertexBuffer.set verts !dst (px + dlx*w) (py + dly*w) u0 1.;
        incr dst;
        VertexBuffer.set verts !dst (px - dlx*w) (py - dly*w) u1 1.;
        incr dst;
        while !i <. ncap do
            let a = float !i / float (ncap -. 1) * Float.pi in
            let ax = Float.cos a * w in
            let ay = Float.sin a * w in
            VertexBuffer.set verts !dst px py 0.5 1.;
            incr dst;
            VertexBuffer.set verts !dst (px - dlx*ax + dx*ay) (py - dly*ax + dy*ay) u0 1.;
            incr dst;
            incr i;
        done
    ;;

    let clamp v min max =
        if v < min then min
        else if v > max then max
        else v
    ;;

    let round_join verts dst (p0 : Point.t) (p1 : Point.t) lw rw lu ru ncap =
        let dlx0 = p0.dy in
        let dly0 = ~-.(p0.dx) in
        let dlx1 = p1.dy in
        let dly1 = ~-.(p1.dx) in
        if PointFlags.has p1.flags ~flag:PointFlags.left then (
            let inner = PointFlags.has p1.flags ~flag:PointFlags.inner_bevel in
            let lx0, ly0, lx1, ly1 = choose_bevel inner p0 p1 lw in
            let a0 = Float.atan2 ~-.dly0 ~-.dlx0 in
            let a1 = Float.atan2 ~-.dly1 ~-.dlx1 in
            let a1 = if a1 > a0 then a1 -. Float.pi*.2. else a1 in

            VertexBuffer.set verts !dst lx0 ly0 lu 1.;
            incr dst;
            VertexBuffer.set verts !dst (p1.x -. dlx0*.rw) (p1.y -. dly0*.rw) ru 1.;
            incr dst;

            let n = clamp
                Float.(ceil(((a0 -. a1) /. pi) *. float ncap) |> int_of_float)
                2
                ncap
            in
            for i=0 to n-1 do
                let u = float i /. (float n -. 1.) in
                let a = a0 +. u*.(a1 -. a0) in
                let rx = p1.x +. Float.cos a *. rw in
                let ry = p1.y +. Float.sin a *. rw in
                VertexBuffer.set verts !dst p1.x p1.y 0.5 1.;
                incr dst;
                VertexBuffer.set verts !dst rx ry ru 1.;
                incr dst;
            done;

            VertexBuffer.set verts !dst lx1 ly1 lu 1.;
            incr dst;
            VertexBuffer.set verts !dst (p1.x -. dlx1*.rw) (p1.y -. dly1*.rw) ru 1.;
            incr dst;
        ) else (
            let inner = PointFlags.has p1.flags ~flag:PointFlags.inner_bevel in
            let rx0, ry0, rx1, ry1 = choose_bevel inner p0 p1 ~-.rw in
            let a0 = Float.atan2 dly0 dlx0 in
            let a1 = Float.atan2 dly1 dlx1 in
            let a1 = if a1 < a0 then a1 +. Float.pi*.2. else a1 in

            VertexBuffer.set verts !dst (p1.x +. dlx0*.rw) (p1.y +. dly0*.rw) lu 1.;
            incr dst;
            VertexBuffer.set verts !dst rx0 ry0 ru 1.;
            incr dst;

            let n = clamp
                Float.(ceil(((a1 -. a0) /. pi) *. float ncap) |> int_of_float)
                2
                ncap
            in
            for i=0 to n-1 do
                let u = float i /. float (n - 1) in
                let a = a0 +. u*.(a1 -. a0) in
                let lx = p1.x +. Float.cos a *. lw in
                let ly = p1.y +. Float.sin a *. lw in
                VertexBuffer.set verts !dst lx ly lu 1.;
                incr dst;
                VertexBuffer.set verts !dst p1.x p1.y 0.5 1.;
                incr dst;
            done;

            VertexBuffer.set verts !dst (p1.x +. dlx1*.rw) (p1.y +. dly1*.rw) lu 1.;
            incr dst;
            VertexBuffer.set verts !dst rx1 ry1 ru 1.;
            incr dst;
        )
    ;;

    let expand_stroke t w fringe line_cap line_join miter_limit =
        let aa = fringe in

        let ncap = curve_divs w Float.pi t.tess_tol in

        let w = w +. aa *. 0.5 in

        let u0, u1 =
            if aa = 0. then (
                0.5, 0.5
            ) else (
                0., 1.
            )
        in

        calculate_joins t w line_join miter_limit;

        let verts = ref (VertexBuffer.num_verts t.cache.verts) in
        let dst = ref !verts in
        DynArray.iter t.cache.paths ~f:(fun path ->
            let get idx =
                DynArray.get t.cache.points (path.first + idx)
            in

            path.fill <- VertexBuffer.Sub.empty;

            dst := !verts;

            let s = ref 0 in
            let e = ref path.count in
            let p0_off = ref (path.count-1) in
            let p1_off = ref 0 in

            if not path.closed then (
                p0_off := 0;
                p1_off := 1;
                s := 1;
                e := path.count-1;

                let p0 = get !p0_off in
                let p1 = get !p1_off in
                let dx = p1.x -. p0.x in
                let dy = p1.y -. p0.y in
                let _, dx, dy = Point.normalize dx dy in
                match line_cap with
                | LineCap.Butt -> dst := butt_cap_start t.cache.verts !dst p0 dx dy w (~-.aa*.0.5) aa u0 u1
                | Square -> dst := butt_cap_start t.cache.verts !dst p0 dx dy w (w -. aa) aa u0 u1
                | Round -> round_cap_start t.cache.verts dst p0 dx dy w ncap u0 u1
                | Default -> ()
            );

            let j = ref !s in
            while !j < !e do
                let p0 = get !p0_off in
                let p1 = get !p1_off in
                if PointFlags.has p1.flags ~flag:PointFlags.bevel
                    || PointFlags.has p1.flags ~flag:PointFlags.inner_bevel then (
                        if line_join = LineJoin.Round then (
                            round_join t.cache.verts dst p0 p1 w w u0 u1 ncap
                        ) else (
                            dst := bevel_join t.cache.verts !dst p0 p1 w w u0 u1
                        );
                ) else (
                    VertexBuffer.set t.cache.verts !dst (p1.x +. (p1.dmx*.w)) (p1.y +. (p1.dmy*.w)) u0 1.;
                    incr dst;
                    VertexBuffer.set t.cache.verts !dst (p1.x -. (p1.dmx*.w)) (p1.y -. (p1.dmy*.w)) u1 1.;
                    incr dst;
                );

                p0_off := !p1_off;
                incr p1_off;
                incr j
            done;

            if path.closed then (
                let v0x, v0y, _, _ = VertexBuffer.get t.cache.verts !verts in
                let v1x, v1y, _, _ = VertexBuffer.get t.cache.verts (!verts+1) in
                VertexBuffer.set t.cache.verts !dst v0x v0y u0 1.;
                incr dst;
                VertexBuffer.set t.cache.verts !dst v1x v1y u1 1.;
                incr dst;
            ) else (
                let p0 = get !p0_off in
                let p1 = get !p1_off in
                (* add line cap *)
                let dx = p1.x -. p0.x in
                let dy = p1.y -. p0.y in
                let _, dx, dy = Point.normalize dx dy in
                match line_cap with
                | Butt -> dst := butt_cap_end t.cache.verts !dst p1 dx dy w (~-.aa*.0.5) aa u0 u1
                | Square -> dst := butt_cap_end t.cache.verts !dst p1 dx dy w (w-.aa) aa u0 u1
                | Round -> round_cap_end t.cache.verts dst p1 dx dy w ncap u0 u1
                | Default -> ()
            );

            let len = !dst - !verts in
            path.stroke <- VertexBuffer.Sub.sub t.cache.verts !verts len;

            verts := !dst;
        );
    ;;

    let stroke (t: t) =
        let state = get_state t in
        let scale = Matrix.get_average_scale state.xform in
        let stroke_width = clamp (state.stroke_width *. scale) 0. 200. |> ref in
        let stroke_paint = Paint.copy state.stroke in

        if !stroke_width < t.fringe_width then (
            let alpha = clamp (!stroke_width /. t.fringe_width) 0. 1. in
            let alpha = alpha*.alpha in
            stroke_paint.inner_color <- Color.{
                stroke_paint.inner_color with
                a = stroke_paint.inner_color.a*.alpha;
            };
            stroke_paint.outer_color <- Color.{
                stroke_paint.outer_color with
                a = stroke_paint.outer_color.a*.alpha;
            };
            stroke_width := t.fringe_width;
        );

        (* Apply global alpha *)
        stroke_paint.inner_color <- Color.{
            stroke_paint.inner_color with
            a = stroke_paint.inner_color.a*.state.alpha
        };
        stroke_paint.outer_color <- Color.{
            stroke_paint.outer_color with
            a = stroke_paint.outer_color.a*.state.alpha;
        };

        Path.flatten t;

        let stroke_width = !stroke_width in
        if Impl.edge_antialias t.impl && state.shape_anti_alias then (
            expand_stroke t (stroke_width*.0.5) t.fringe_width state.line_cap state.line_join state.miter_limit;
        ) else (
            expand_stroke t (stroke_width*.0.5) 0.0 state.line_cap state.line_join state.miter_limit
        );

        Impl.stroke t.impl
            ~paint:stroke_paint
            ~composite_op:state.composite_operation
            ~scissor:state.scissor
            ~fringe:t.fringe_width
            ~stroke_width
            ~paths:t.cache.paths;

            (*
        (* Count triangles *)
        DynArray.iter t.cache.paths ~f:(fun path ->
            t.stroke_tri_count <- t.stroke_tri_count + (VertexBuffer.Sub.num_verts path.stroke) - 2;
            t.draw_call_count <- t.draw_call_count + 1;
        );
        *)
    ;;

    let fill (t : t) =
        let state = get_state t in
        let fill_paint = Paint.copy state.fill in
        Path.flatten t;
        (* TODO - finish *)
        if Impl.edge_antialias t.impl && state.shape_anti_alias then (
            expand_fill t t.fringe_width LineJoin.Miter 2.4
        ) else (
            expand_fill t 0. LineJoin.Miter 2.4
        );

        fill_paint.inner_color <- Color.{
            fill_paint.inner_color with
            a = fill_paint.inner_color.a *. state.alpha;
        };
        fill_paint.outer_color <- Color.{
            fill_paint.outer_color with
            a = fill_paint.outer_color.a *. state.alpha;
        };

        Impl.fill t.impl
            ~paint:fill_paint
            ~composite_op:state.composite_operation
            ~scissor:state.scissor
            ~fringe:t.fringe_width
            ~bounds:t.cache.bounds
            ~paths:t.cache.paths
            ~verts:t.cache.verts
        ;
    ;;

    let end_frame t =
        Impl.flush t.impl t.cache.verts;

        if t.font_image_idx <> 0 then (
            (* TODO - add font/text stuff *)
        );
    ;;

    let set_paint_color (p : Paint.t) (color : Color.t) : unit =
        Matrix.identity p.xform;
        p.radius <- 0.;
        p.feather <- 1.;
        p.inner_color <- color;
        p.outer_color <- color;
    ;;

    module Paint = struct
        type ctx = t
        type t = Graphv_core_lib.Paint.t

        let linear_gradient (_t : ctx) ~sx ~sy ~ex ~ey ~icol ~ocol : t =
            let open FloatOps in
            let paint = Paint.create() in
            Matrix.identity paint.xform;
            let dx = ex - sx in
            let dy = ey - sy in
            let d = Float.sqrt (dx*dx + dy*dy) in
            let dx, dy =
                if d > 0.0001 then (
                    dx / d, dy / d
                ) else (
                    0., 1.
                )
            in

            let xform =  paint.xform in
            let large = 1e5 in
            xform.m0 <- dy;
            xform.m1 <- ~-.dx;
            xform.m2 <- dx;
            xform.m3 <- dy;
            xform.m4 <- (sx - dx*large);
            xform.m5 <- (sy - dy*large);

            paint.extent <- large, large + d*0.5;
            paint.radius <- 0.;
            paint.feather <- Float.max 1. d;
            paint.inner_color <- icol;
            paint.outer_color <- ocol;
            paint
        ;;

        let box_gradient (_t : ctx) ~x ~y ~w ~h ~r ~f ~icol ~ocol : t =
            let open FloatOps in
            let paint = Paint.create() in
            Matrix.identity paint.xform;
            let xform = paint.xform in
            xform.m4 <- x+w*0.5;
            xform.m5 <- y+h*0.5;

            paint.extent <- w*0.5, h*0.5;
            paint.radius <- r;
            paint.feather <- Float.max 1. f;
            paint.inner_color <- icol;
            paint.outer_color <- ocol;
            paint
        ;;

        let radial_gradient (_t : ctx) ~cx ~cy ~in_radius ~out_radius ~icol ~ocol : t =
            let open FloatOps in
            let paint = Paint.create() in
            Matrix.identity paint.xform;
            let xform = paint.xform in
            xform.m4 <- cx;
            xform.m5 <- cy;

            let r = (in_radius + out_radius)*0.5 in
            let f = (out_radius - in_radius) in

            paint.extent <- r, r;
            paint.radius <- r;
            paint.feather <- Float.max 1. f;
            paint.inner_color <- icol;
            paint.outer_color <- ocol;
            paint
        ;;

        let image_pattern (_t : ctx) ~cx ~cy ~w ~h ~angle ~image ~alpha : t =
            let paint = Paint.create() in
            Matrix.rotate paint.xform ~angle;

            let xform = paint.xform in
            xform.m4 <- cx;
            xform.m5 <- cy;

            paint.extent <- w, h;
            paint.image <- image;

            let color = Color.rgbaf ~r:1. ~g:1. ~b:1. ~a:alpha in
            paint.inner_color <- color;
            paint.outer_color <- color;

            paint
        ;;
    end

    let set_fill_color t ~color =
        let state = get_state t in
        set_paint_color state.fill color
    ;;

    let set_fill_paint t ~paint =
        let state = get_state t in
        state.fill <- Graphv_core_lib.Paint.copy paint;
        Matrix.multiply ~dst:state.fill.xform ~src:state.xform
    ;;

    let set_stroke_color t ~color =
        let state = get_state t in
        set_paint_color state.stroke color
    ;;

    let set_stroke_paint t ~paint =
        let state = get_state t in
        state.stroke <- Graphv_core_lib.Paint.copy paint;
        Matrix.multiply ~dst:state.stroke.xform ~src:state.xform
    ;;

    let quantize a d =
        (Float.floor (a /. d +. 0.5)) *. d
    ;;

    let get_font_scale (state : State.t) =
        let avg = Matrix.get_average_scale state.xform in
        let avg = quantize avg 0.01 in
        min avg 4.
    ;;

    let render_text t verts off =
        let state = get_state t in
        let paint = Graphv_core_lib.Paint.copy state.fill in

        paint.image <- DynArray.get t.font_images t.font_image_idx;

        paint.inner_color <- Color.{
            paint.inner_color with
            a = paint.inner_color.a *. state.alpha;
        };
        paint.outer_color <- Color.{
            paint.outer_color with
            a = paint.outer_color.a *. state.alpha;
        };

        let len = VertexBuffer.num_verts verts - off in
        Impl.triangles t.impl
            ~paint
            ~composite_op:state.composite_operation
            ~scissor:state.scissor
            ~fringe:t.fringe_width
            ~vertices:(VertexBuffer.Sub.sub verts off len)
            ;

        t.draw_call_count <- t.draw_call_count + 1;
        t.text_tri_count <- t.text_tri_count + len / 3;
    ;;

    let flush_text_texture t =
        match FontContext.validate_texture t.fs with
        | None -> ()
        | Some (x0, y0, x1, y1) ->
            let image = DynArray.get t.font_images t.font_image_idx in
            if image <> 0 then (
                let data, _w, _h = FontContext.get_texture_data t.fs in
                let x = x0
                and y = y0
                and w = x1 - x0
                and h = y1 - y0 in
                Impl.update_texture t.impl ~image ~x ~y ~w ~h ~data |> ignore
            )
    ;;

    module Text = struct
        type bounds = {
            box : Bounds.t;
            advance : float;
        }
        type font = FontContext.font

        let set_font_attributes t (state : State.t) scale =
            FontContext.set_size t.fs (state.font_size*.scale);
            FontContext.set_spacing t.fs (state.letter_spacing*.scale);
            FontContext.set_blur t.fs (state.font_blur*.scale);
            FontContext.set_align t.fs state.text_align;
            FontContext.set_font t.fs state.font_id;
        ;;

        let transform_points verts (xm : Matrix.t) (q : FontContext.Quad.t) inv_scale =
            let open FloatOps in
            (* Transform corners *)
            let qx0 = q.x0*inv_scale in
            let qy0 = q.y0*inv_scale in

            let c0 = qx0*xm.m0 + qy0*xm.m2 + xm.m4 in
            let c1 = qx0*xm.m1 + qy0*xm.m3 + xm.m5 in

            let at = VertexBuffer.num_verts verts in
            VertexBuffer.set verts at c0 c1 q.s0 q.t0;

            let qx1 = q.x1*inv_scale in
            let qy1 = q.y1*inv_scale in
            let c4 = qx1*xm.m0 + qy1*xm.m2 + xm.m4 in
            let c5 = qx1*xm.m1 + qy1*xm.m3 + xm.m5 in
            VertexBuffer.set verts (at+.1) c4 c5 q.s1 q.t1;

            let c2 = qx1*xm.m0 + qy0*xm.m2 + xm.m4 in
            let c3 = qx1*xm.m1 + qy0*xm.m3 + xm.m5 in
            VertexBuffer.set verts (at+.2) c2 c3 q.s1 q.t0;
            VertexBuffer.set verts (at+.3) c0 c1 q.s0 q.t0;

            let c6 = qx0*xm.m0 + qy1*xm.m2 + xm.m4 in
            let c7 = qx0*xm.m1 + qy1*xm.m3 + xm.m5 in

            (* Create triangles *)
            VertexBuffer.set verts (at+.4) c6 c7 q.s0 q.t1;
            VertexBuffer.set verts (at+.5) c4 c5 q.s1 q.t1;
        ;;

        let quad = FontContext.Quad.empty()

        let text_w t ~x ~y ?(start=0) ?end_ str =
            let open FloatOps in
            let state = get_state t in

            let scale = get_font_scale state * t.device_pixel_ratio in
            let inv_scale = 1. / scale in
            let is_flipped = Matrix.is_flipped state.xform in

            let end_ = match end_ with
                    | None -> String.length str
                    | Some len -> len
            in

            set_font_attributes t state scale;

            let verts = VertexBuffer.num_verts t.cache.verts in
            let iter = FontContext.iter_init t.fs
                (x*scale)
                (y*scale)
                ~start
                ~end_
                str
                FontContext.GlyphBitmap.required
            in
            let rec loop iter =
                let q = FontContext.iter_next t.fs iter quad in
                match q with
                | false -> iter
                | true ->
                    if is_flipped then (
                        (* Swap quad *)
                        let y0 = quad.y0 in
                        let t0 = quad.t0 in
                        quad.y0 <- quad.y1;
                        quad.t0 <- quad.t1;
                        quad.y1 <- y0;
                        quad.t1 <- t0;
                    );
                    transform_points t.cache.verts state.xform quad inv_scale;
                    loop iter
            in
            let iter = loop iter in

            (* TODO - once per frame? *)
            flush_text_texture t;
            render_text t t.cache.verts verts;

            FontContext.Iter.next_x iter  / scale
        ;;

        let text t ~x ~y ?start ?end_ str =
            text_w t ~x ~y ?start ?end_ str |> ignore

        type metrics = {
            ascender : float;
            descender : float;
            line_height : float;
        }

        let metrics t =
            let state = get_state t in
            let scale = get_font_scale state *. t.device_pixel_ratio in
            set_font_attributes t state scale;
            let metrics = FontContext.vert_metrics t.fs in
            {
                ascender = metrics.ascender;
                descender = metrics.descender;
                line_height = metrics.line_height
            }
        ;;

        type text_row = {
            start_index : int;
            end_index : int;
            width : float;
            minx : float;
            maxx : float;
            next : int;
        }

        let empty_row = {
            start_index = 0;
            end_index = 0;
            width = 0.;
            minx = 0.;
            maxx = 0.;
            next = 0;
        }

        let make_empty_rows count =
            Array.make count empty_row

        type codepoint_type = Space
                            | Newline
                            | Char
                            | CJK_char

        let codepoint_type = function
            | 9 | 11 | 12 | 13 | 32 -> Space
            | 0x85 | 10 -> Newline
            | ch ->
                if (ch >= 0x4E00 && ch <= 0x9FFFF)
                    || (ch >= 0x3000 && ch <= 0x30FF)
                    || (ch >= 0xFF00 && ch <= 0xFFEF)
                    || (ch >= 0x1100 && ch <= 0x11FF)
                    || (ch >= 0x3130 && ch <= 0x318F)
                    || (ch >= 0xAC00 && ch <= 0xD7AF) then (
                        CJK_char
                ) else (
                    Char
                )
        ;;

        let break_lines t ~break_width ~max_rows ?start ?end_ ~lines text =
            let open FloatOps in
            let state = get_state t in
            let scale = get_font_scale state * t.device_pixel_ratio in
            let inv_scale = 1. / scale in

            set_font_attributes t state scale;

            let module F = FontContext in
            let module FI = F.Iter in

            let break_row_width = break_width * scale in
            let iter = F.iter_init t.fs 0. 0. ?start ?end_ text F.GlyphBitmap.optional in
            let nrows = ref 0 in
            let row_start_x = ref 0. in
            let row_width = ref 0. in
            let row_min_x = ref 0. in
            let row_max_x = ref 0. in
            let row_start = ref ~-1 in
            let row_end = ref ~-1 in
            let word_start = ref ~-1 in
            let word_start_x = ref 0. in
            let word_min_x = ref 0. in
            let break_end = ref ~-1 in
            let break_width = ref 0. in
            let break_max_x = ref 0. in

            begin try
                let rec loop (ptype : codepoint_type) : unit =
                    let res = F.iter_next t.fs iter quad in
                    match res with
                    | false -> ()
                    | true ->
                        let type_ = codepoint_type (FI.codepoint iter) in
                        begin match type_ with
                        | Newline ->
                            (* Always handle new lines *)
                            let row : text_row = {
                                start_index = (if !row_start >=. 0 then !row_start else FI.start iter);
                                end_index = (if !row_end >=. 0 then !row_end else FI.start iter);
                                width = !row_width * inv_scale;
                                minx = !row_min_x * inv_scale;
                                maxx = !row_max_x * inv_scale;
                                next = FI.next iter;
                            } in
                            lines.(!nrows) <- row;
                            incr nrows;
                            if !nrows >=. max_rows then (
                                raise Exit
                            );

                            (* set null break point *)
                            break_end := !row_start;
                            break_width := 0.;
                            break_max_x := 0.;
                            (* skip white space until new row *)
                            row_start := ~-1;
                            row_end := ~-1;
                            row_width := 0.;
                            row_min_x := 0.;
                            row_max_x := 0.;
                        | _ ->
                            if !row_start =. ~-1 then (
                                if Stdlib.(type_ = Char || type_ = CJK_char) then (
                                    (* skip white space until the beginning of the line *)
                                    row_start_x := FI.x iter;
                                    row_start := FI.start iter;
                                    row_end := FI.next iter;
                                    row_width := FI.next_x iter - !row_start_x;
                                    row_min_x := quad.x0 - !row_start_x;
                                    row_max_x := quad.x1 - !row_start_x;
                                    word_start := FI.start iter;
                                    word_start_x := FI.x iter;
                                    word_min_x := quad.x0 - !row_start_x;
                                    break_end := !row_start;
                                    break_width := 0.;
                                    break_max_x := 0.;
                                )
                            ) else (
                                let next_width = FI.next_x iter - !row_start_x in

                                (* Track last beginning of word *)
                                if Stdlib.((ptype = Space && (type_ = Char || type_ = CJK_char))
                                    || type_ = CJK_char) then
                                (
                                    word_start := FI.start iter;
                                    word_start_x := FI.x iter;
                                    word_min_x := quad.x0;
                                );

                                (* Track last non-whitespace character *)
                                if Stdlib.(type_ = Char || type_ = CJK_char) then (
                                    row_end := FI.next iter;
                                    row_width := FI.next_x iter - !row_start_x;
                                    row_max_x := quad.x1 - !row_start_x;
                                );

                                (* Track last end of word *)
                                if Stdlib.(((ptype = Char || ptype = CJK_char) && type_ = Space)
                                    || (type_ = CJK_char)) then
                                (
                                    break_end := FI.next iter;
                                    word_start := FI.next iter;
                                    break_width := !row_width;
                                    break_max_x := !row_max_x;
                                );

                                (* Break to new line when a character is beyond break width *)
                                if Stdlib.(
                                    (type_ = Char || type_ = CJK_char) && next_width > break_row_width) then (

                                    (* The run length is too long, need to break to a new line *)
                                    if !break_end =. !row_start then (

                                        (* Current word is longer than the row length, break it *)
                                        let row : text_row = {
                                            start_index = !row_start;
                                            end_index = FI.start iter;
                                            width = !row_width * inv_scale;
                                            minx = !row_min_x * inv_scale;
                                            maxx = !row_max_x * inv_scale;
                                            next = FI.start iter;
                                        } in
                                        lines.(!nrows) <- row;
                                        incr nrows;
                                        if !nrows >=. max_rows then (
                                            raise Exit
                                        );

                                        row_start_x := FI.x iter;
                                        row_start := FI.start iter;
                                        row_end := FI.next iter;
                                        row_width := FI.next_x iter - !row_start_x;
                                        row_min_x := quad.x0 - !row_start_x;
                                        row_max_x := quad.x1 - !row_start_x;
                                        word_start := FI.start iter;
                                        word_start_x := FI.x iter;
                                        word_min_x := quad.x0 - !row_start_x;
                                    ) else (
                                        let row : text_row = {
                                            start_index = !row_start;
                                            end_index = !break_end;
                                            width = !break_width * inv_scale;
                                            minx = !row_min_x * inv_scale;
                                            maxx = !break_max_x * inv_scale;
                                            next = !word_start;
                                        } in
                                        lines.(!nrows) <- row;
                                        incr nrows;
                                        if !nrows >=. max_rows then (
                                            raise Exit
                                        );

                                        row_start_x := !word_start_x;
                                        row_start := !word_start;
                                        row_end := FI.next iter;
                                        row_width := FI.next_x iter - !row_start_x;
                                        row_min_x := !word_min_x - !row_start_x;
                                        row_max_x := quad.x1 - !row_start_x;
                                    );
                                    break_end := !row_start;
                                    break_width := 0.;
                                    break_max_x := 0.;
                                )
                        );
                    end;
                    loop type_
                in
                loop Space;

                let end_ = match end_ with
                         | None -> String.length text
                         | Some e -> e
                in

                let row_start =
                    if !row_start =. end_ then ~-1
                    else !row_start
                in

                if not (row_start =. ~-1) then (
                    let row : text_row = {
                        start_index = row_start;
                        end_index = !row_end;
                        width = !row_width * inv_scale;
                        minx = !row_min_x * inv_scale;
                        maxx = !row_max_x * inv_scale;
                        next = end_;
                    } in
                    lines.(!nrows) <- row;
                    incr nrows;
                );

                !nrows
            with Exit ->
                !nrows
            end;
        ;;

        type glyph_position = {
            index : int;
            x : float;
            min_x : float;
            max_x : float;
        }

        let empty_glyph_position = {
            index = 0;
            x = 0.;
            min_x = 0.;
            max_x = 0.;
        }

        let glyph_positions (t : t) ~x ~y ?start ?end_ ~glyphs text : int =
            let open FloatOps in
            let state = get_state t in
            let scale = get_font_scale state * t.device_pixel_ratio in
            let inv_scale = 1. / scale in

            set_font_attributes t state scale;

            let module F = FontContext in
            let iter = F.iter_init t.fs (x*scale) (y*scale) ?start ?end_ text F.GlyphBitmap.optional in
            let _npos = ref 0 in
            let rec loop idx =
                let res = F.iter_next t.fs iter quad in
                match res with
                | false -> idx
                | true ->
                    let pos = {
                        index = F.Iter.start iter;
                        x = F.Iter.x iter * inv_scale;
                        min_x = (Float.min (F.Iter.x iter) quad.x0) * inv_scale;
                        max_x = (Float.max (F.Iter.next_x iter) quad.x1) * inv_scale;
                    } in
                    glyphs.(idx) <- pos;
                    loop (idx+.1)
            in
            loop 0
        ;;

        let bounds t ~x ~y ?start ?end_ str =
            let state = get_state t in
            let scale = get_font_scale state *. t.device_pixel_ratio in
            let inv_scale = 1. /. scale in
            set_font_attributes t state scale;

            let advance, bounds = FontContext.bounds t.fs (x*.scale) (y*.scale) ?off:start ?end_ str in
            let ymin, ymax = FontContext.line_bounds t.fs (y*.scale) in

            {
                advance  = advance *. inv_scale;
                box = {
                    xmin = bounds.xmin *. inv_scale;
                    xmax = bounds.xmax *. inv_scale;
                    ymin = ymin *. inv_scale;
                    ymax = ymax *. inv_scale;
                }
            }
        ;;

        let lines = [|empty_row; empty_row|]

        let box_bounds t ~x ~y ~break_width ?(start=0) ?end_ text =
            let open FloatOps in
            let state = get_state t in
            let scale = get_font_scale state * t.device_pixel_ratio in
            let inv_scale = 1. / scale in

            let module F = FontContext in
            let old_align = state.text_align in
            let halign = Align.h_align state.text_align in
            let valign = Align.v_align state.text_align in

            let min_x = ref x in
            let max_x = ref x in
            let min_y = ref y in
            let max_y = ref y in

            let line_h = (metrics t).line_height in

            state.text_align <- Align.(left lor valign);

            set_font_attributes t state scale;

            let y = ref y in
            let rmin_y, rmax_y = F.line_bounds t.fs 0. in
            let rmin_y = rmin_y * inv_scale in
            let rmax_y = rmax_y * inv_scale in

            let start = ref start in

            let rec loop () =
                match break_lines t ~break_width ~max_rows:2 ~start:!start ?end_ ~lines text with
                | 0 -> ()
                | count ->
                    for i=0 to count-.1 do
                        let line = lines.(i) in
                        let dx =
                            if Align.(has halign ~flag:left) then (
                                0.
                            ) else if Align.(has halign ~flag:center) then (
                                break_width*0.5 - line.width*0.5
                            ) else if Align.(has halign ~flag:right) then (
                                break_width - line.width
                            ) else 0.
                        in
                        let rmin_x = x + line.minx + dx in
                        let rmax_x = x + line.maxx + dx in
                        min_x := Float.min !min_x rmin_x;
                        max_x := Float.max !max_x rmax_x;
                        min_y := Float.min !min_y (!y + rmin_y);
                        max_y := Float.max !max_y (!y + rmax_y);

                        y := !y + line_h * state.line_height;
                        start := line.next;
                    done;
                    loop()
            in
            loop ();
            state.text_align <- old_align;
            Bounds.{
                xmin = !min_x;
                ymin = !min_y;
                xmax = !max_x;
                ymax = !max_y;
            }
        ;;

        let create t ~name ~file =
            FontContext.add_font t.fs name file
        ;;

        let set_size t ~size =
            let state = get_state t in
            state.font_size <- size

        let set_blur t ~blur =
            let state = get_state t in
            state.font_blur <- blur

        let set_letter_spacing t ~spacing =
            let state = get_state t in
            state.letter_spacing <- spacing

        let set_align t ~align =
            let state = get_state t in
            state.text_align <- align

        let set_line_height t ~height =
            let state = get_state t in
            state.line_height <- height;
        ;;

        let find_font t ~name =
            FontContext.find_font t.fs name

        let set_font_face t ~name =
            let state = get_state t in
            match FontContext.find_font t.fs name with
            | None -> ()
            | Some id -> state.font_id <- id
        ;;

        let set_font_face_id t ~id =
            let state = get_state t in
            state.font_id <- id
        ;;

        let text_box t ~x ~y ~break_width ?(start=0) ?end_ str =
            let open FloatOps in
            let state = get_state t in
            let old_align = state.text_align in
            let halign = Align.h_align state.text_align in
            let valign = Align.v_align state.text_align in

            let metrics = metrics t in

            state.text_align <- Align.(left lor valign);

            let start = ref start in
            let y = ref y in

            let rec loop () =
                match break_lines t ~break_width ~max_rows:2 ~start:!start ?end_ ~lines str with
                | 0 -> ()
                | count ->
                    for i=0 to count-.1 do
                        let line = lines.(i) in
                        let s = line.start_index in
                        let e = line.end_index in
                        if Align.(has halign ~flag:left) then (
                            text t ~x ~y:!y ~start:s ~end_:e str
                        ) else if Align.(has halign ~flag:center) then (
                            let x = x + break_width*0.5 - line.width*0.5 in
                            text t ~x ~y:!y ~start:s ~end_:e str
                        ) else if Align.(has halign ~flag:right) then (
                            let x = x + break_width - line.width in
                            text t ~x ~y:!y ~start:s ~end_:e str
                        );
                        y := !y + metrics.line_height * state.line_height;
                        start := line.next;
                    done;
                    loop()
            in
            loop();

            state.text_align <- old_align
        ;;

        let add_fallback_id t ~font ~fallback =
            FontContext.add_fallback_id t.fs ~font ~fallback
        ;;

        let add_fallback t ~name ~fallback =
            FontContext.add_fallback t.fs ~name ~fallback
        ;;

        let reset_fallback_id t ~font =
            FontContext.reset_fallback_id t.fs ~font
        ;;

        let reset_fallback t ~name =
            FontContext.reset_fallback t.fs ~name
        ;;

    end

    let create ~flags arg =
        let t = {
            impl = Impl.create ~flags arg |> opt_exn;
            commands = DynArray.create 128 Command.Close;
            command_x = 0.;
            command_y = 0.;
            states = DynArray.create 10 State.(create());
            cache = PathCache.create();
            tess_tol = 0.;
            dist_tol = 0.;
            fringe_width = 0.;
            device_pixel_ratio = 1.;
            fs = FontContext.create();
            font_images = DynArray.create 1 0;
            draw_call_count = 0;
            fill_tri_count = 0;
            stroke_tri_count = 0;
            text_tri_count = 0;
            font_image_idx = 0;
        } in
        let data, w, h = FontContext.get_texture_data t.fs in
        let value = Impl.create_texture t.impl ~type_:`Alpha ~w ~h ~flags:ImageFlags.no_flags ~data in
        begin match value with
        | Some image ->
            DynArray.add t.font_images image;
        | None -> failwith "Couldn't create font texture"
        end;
        save t;
        reset t;
        set_device_pixel_ratio t 1.;
        t
    ;;

end
