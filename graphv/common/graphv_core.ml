open Graphv_core_lib

module Context = Context

module Make
    (Impl : Graphv_core_lib.Impl.S)
    (Font : Graphv_core_lib.Font_impl.S with type data := Impl.Buffer.UByte.t)
    : Context.S
        with module Buffer = Impl.Buffer
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
            points : Point.t;
            mutable paths : IPath.t DynArray.t;
            verts : VertexBuffer.t;
            mutable bounds : Bounds.t;
        }

        let create () = {
            points = Point.create();
            paths = DynArray.init 10 IPath.create;
            verts = VertexBuffer.create();
            bounds = Bounds.empty;
        }

        let clear t =
            Point.clear t.points;
            DynArray.clear t.paths;
    end

    type t = {
        impl : Impl.t;
        commands : Command.t DynArray.t;
        mutable command_x : float;
        mutable command_y : float;
        tesselate_afd : bool;
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
            let open FloatOps in
            let state = get_state t in

            let w = max w 0. in
            let h = max h 0. in

            Matrix.identity state.scissor.xform;
            let m = state.scissor.xform in
            m.m4 <- x+w*0.5;
            m.m5 <- y+h*0.5;
            Matrix.multiply ~dst:state.scissor.xform ~src:state.xform;

            state.scissor <- Scissor.{
                state.scissor with
                extent_0 = w*0.5;
                extent_1 = h*0.5;
            };
        ;;

        let intersect_rects ax ay aw ah bx by bw bh =
            let open FloatOps in
            let minx = max ax bx in
            let miny = max ay by in
            let maxx = min (ax+aw) (bx+bw) in
            let maxy = min (ay+ah) (by+bh) in
            minx, miny, (max 0. (maxx-minx)), (max 0. (maxy - miny))
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
        let npoints = Point.length t.cache.points in
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

    let [@inline always] last_point t = Point.last t.cache.points

    let add_point t x y flags =
        let path = last_path t in
        let points = t.cache.points in

        let insert () =
            (*let point = DynArray.steal t.cache.points Point.empty in
            Point.reset point x y flags;
              *)
            Point.add points x y flags;
            path.count <- path.count + 1;
        in

        if path.count > 0 && Point.length t.cache.points > 0 then (
            if Point.last_equals points x y t.dist_tol then (
                Point.add_last_flag points flags
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
            let [@inlined] dx = x4 - x1 in
            let [@inlined] dy = y4 - y1 in
            let [@inlined] d2 = fabs ((x2 - x4)*dy - (y2 - y4)*dx) in
            let [@inlined] d3 = fabs ((x3 - x4)*dy - (y3 - y4)*dx) in

            if (d2+d3)*(d2+d3) < t.tess_tol * (dx*dx + dy*dy) then (
                add_point t x4 y4 typ
            ) else (
                let [@inlined] x23 = (x2+x3)*0.5 in
                let [@inlined] x34 = (x3+x4)*0.5 in
                let [@inlined] x234 = (x23 + x34)*0.5 in

                let [@inlined] x12 = (x1+x2)*0.5 in
                let [@inlined] x123 = (x12+x23)*0.5 in
                let [@inlined] x1234 = (x123 + x234)*0.5 in

                let [@inlined] y23 = (y2+y3)*0.5 in
                let [@inlined] y34 = (y3+y4)*0.5 in
                let [@inlined] y234 = (y23 + y34)*0.5 in

                let [@inlined] y12 = (y1+y2)*0.5 in
                let [@inlined] y123 = (y12+y23)*0.5 in
                let [@inlined] y1234 = (y123 + y234)*0.5 in

                tesselate_bezier t x1 y1 x12 y12 x123 y123 x1234 y1234 (level +. 1) PointFlags.no_flags;
                tesselate_bezier t x1234 y1234 x234 y234 x34 y34 x4 y4 (level +. 1) typ;
            )
        )
    ;;

    let tesselate_bezier_afd t x1 y1 x2 y2 x3 y3 x4 y4 typ =
        (* Code adapted from
            https://github.com/memononen/nanovg/pull/420
            https://dl.acm.org/doi/10.1145/37401.37416
         *)
        let open FloatOps in

        (* Power basis *)
        let ax = ~-.x1 + 3.*x2 - 3.*x3 + x4 in
        let ay = ~-.y1 + 3.*y2 - 3.*y3 + y4 in
        let bx = 3.*x1 - 6.*x2 + 3.*x3 in
        let by = 3.*y1 - 6.*y2 + 3.*y3 in
        let cx = ~-.3.*x1 + 3.*x2 in
        let cy = ~-.3.*y1 + 3.*y2 in

        let px = ref x1 in
        let py = ref y1 in
        let dx = ref (ax + bx + cx) in
        let dy = ref (ay + by + cy) in
        let ddx = ref (6.*ax + 2.*bx) in
        let ddy = ref (6.*ay + 2.*by) in
        let dddx = ref (6.*ax) in
        let dddy = ref (6.*ay) in

        let afd_one = 1 lsl 10 in
        let i = ref 0 in
        let dt = ref afd_one in
        let tol = t.tess_tol*12. in

        while !i <. afd_one do
            (* flatness measure *)
            let d = ref (!ddx * !ddx + !ddy * !ddy + !dddx * !dddx + !dddy * !dddy) in

            while (!d > tol && !dt >. 1) || (!i +. !dt >. afd_one) do
                dx := 0.5 * !dx - (1./8.) * !ddx + (1./16.) * !dddx;
                dy := 0.5 * !dy - (1./8.) * !ddy + (1./16.) * !dddy;
                ddx := (1./4.) * !ddx - (1./8.) * !dddx;
                ddy := (1./4.) * !ddy - (1./8.) * !dddy;
                dddx := (1./8.) * !dddx;
                dddy := (1./8.) * !dddy;

                dt := !dt lsr 1;

                d := (!ddx * !ddx + !ddy * !ddy + !dddx * !dddx + !dddy * !dddy);
            done;

            while (!d > 0. && !d < tol/10. && !dt <. afd_one) && (!i+.2*. !dt <=. afd_one) do
                dx := 2. * !dx + !ddx;
                dy := 2. * !dy + !ddy;
                ddx := 4. * !ddx + 4. * !dddx;
                ddy := 4. * !ddy + 4. * !dddy;
                dddx := 8. * !dddx;
                dddy := 8. * !dddy;

                dt := !dt lsl 1;

                d := !ddx * !ddx + !ddy * !ddy + !dddx * !dddx + !dddy * !dddy;
            done;

            px := !px + !dx;
            py := !py + !dy;
            dx := !dx + !ddx;
            dy := !dy + !ddy;
            ddx := !ddx + !dddx;
            ddy := !ddy + !dddy;

            add_point t !px !py (if !i >. 0 then typ else PointFlags.no_flags);

            i := !i +. !dt;
        done;
    ;;

    let poly_area (points : Point.t) offset count =
        let area = ref 0. in
        for i=2 to count-1 do
            area := !area +. Point.triarea2 points offset (offset + i - 1) (offset + i)
        done;
        !area *. 0.5
    ;;

    let poly_reverse (points : Point.t) offset count =
        let flags = points.flags in
        let data = points.data in

        let swap data i j =
          let tmp = Point.get data i in
          Point.set data i (Point.get data j);
          Point.set data j tmp;
        in

        (* Swap flags *)
        let i = ref 0 in
        let j = ref (count - 1) in
        while !i < !j do
            let tmp = Array.unsafe_get flags (offset + !i) in
            Array.unsafe_set flags (offset + !i) (Array.unsafe_get flags (offset + !j));
            Array.unsafe_set flags (offset + !j) tmp;
            incr i;
            decr j;
        done;

        let offset = offset*Point.count in
        let i = ref offset in
        let j = ref (offset + ((count - 1)*Point.count)) in
        while !i < !j do
            swap data !i !j;
            swap data (!i+1) (!j+1);
            swap data (!i+2) (!j+2);
            swap data (!i+3) (!j+3);
            swap data (!i+4) (!j+4);
            swap data (!i+5) (!j+5);
            swap data (!i+6) (!j+6);
            i := !i + Point.count;
            j := !j - Point.count;
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
                let x, y = (Matrix.transform_point[@inlined]) xform x y in
                Move_to {x;y}
            | Line_to {x;y} ->
                let x, y = (Matrix.transform_point[@inlined]) xform x y in
                Line_to {x;y}
            | Bezier_to {c1x;c1y;c2x;c2y;x;y} ->
                let x, y = (Matrix.transform_point[@inlined]) xform x y in
                let c1x, c1y = (Matrix.transform_point[@inlined]) xform c1x c1y in
                let c2x, c2y = (Matrix.transform_point[@inlined]) xform c2x c2y in
                Bezier_to {c1x;c1y;c2x;c2y;x;y}
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

        let flatten t =
            let open FloatOps in
            if DynArray.length t.cache.paths =. 0 then (
                for i=0 to DynArray.length t.commands -. 1 do
                    match DynArray.get t.commands i with
                    | Move_to {x;y} ->
                        add_path t;
                        add_point t x y PointFlags.corner;
                    | Line_to {x;y} ->
                        add_point t x y PointFlags.corner;
                    | Bezier_to {c1x; c1y; c2x; c2y; x; y} ->
                        let pts = t.cache.points in
                        let last_x = Point.get_last_x pts in
                        let last_y = Point.get_last_y pts in
                        if t.tesselate_afd then (
                          tesselate_bezier_afd t last_x last_y c1x c1y c2x c2y x y PointFlags.corner
                        ) else (
                          tesselate_bezier t last_x last_y c1x c1y c2x c2y x y 0 PointFlags.corner
                        )
                    | Close -> close_path t
                    | Winding w -> path_winding t w
                done;

                let [@inlined] xmin = ref 1e6 in
                let [@inlined] ymin = ref 1e6 in
                let [@inlined] xmax = ref ~-.1e6 in
                let [@inlined] ymax = ref ~-.1e6 in

                let points = t.cache.points in
                (* Wish we had sub arrays for DynArray *)
                for i=0 to DynArray.length t.cache.paths -. 1 do
                    let path = DynArray.get t.cache.paths i in
                    (* If first and last points are the same, remove last, mark as closed *)
                    let p1 = path.first in
                    let p0 = p1 +. (path.count -. 1) in
                    let p0 =
                        if Point.equal_index points p0 p1 t.dist_tol then (
                            path.count <- path.count -. 1;
                            path.closed <- true;
                            p0 -. 1
                        ) else 
                          p0
                    in

                    (* Enforce winding *)
                    if path.count >. 2 then (
                        let area = poly_area points path.first path.count in
                        let reverse = 
                            match path.winding with
                            | Winding.CCW -> area < 0.
                            | CW -> area > 0.
                        in
                        if reverse then (
                            poly_reverse points path.first path.count
                        )
                    );

                    let p1 = ref p1 in
                    let p0 = ref p0 in
                    for _=0 to path.count-.1 do
                        let open FloatOps in
                        (* This calc is good *)
                        let x, y = Point.get_xy points !p0 in

                        (* Update bounds *)
                        xmin := min !xmin x;
                        ymin := min !ymin y;
                        xmax := max !xmax x;
                        ymax := max !ymax y;

                        (Point.assign_dx_dy[@inlined]) points !p0 !p1;
                        (Point.normalize_pt[@inlined]) points !p0;

                        p0 := !p1;
                        incr p1;
                    done;
                done;

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
        let open FloatOps in

        let iw =
            if w > 0. then (
                1.0 / w
            ) else (
                0.
            )
        in

        let data = t.cache.points.data in
        let flags = t.cache.points.flags in
        for i=0 to DynArray.length t.cache.paths -. 1 do
            let path = DynArray.get t.cache.paths i in
            let [@inline always] get idx =
                (path.first +. idx)
            in
            let p0_off = ref (path.count -. 1) in
            let p1_off = ref 0 in
            let left = ref 0 in

            path.nbevel <- 0;

            for _=0 to path.count-.1 do
                let p0 = get !p0_off in
                let p1 = get !p1_off in

                let dlx0 = Point.get_dy data p0 in
                let dly0 = ~-.(Point.get_dx data p0) in
                let dlx1 = Point.get_dy data p1 in
                let dly1 = ~-.(Point.get_dx data p1) in

                (* Calculate extrusions *)
                (* these are right *)
                let dmx = (dlx0 + dlx1) * 0.5 in
                let dmy = (dly0 + dly1) * 0.5 in
                Point.set_dmx data p1 dmx;
                Point.set_dmy data p1 dmy;

                let dmr2 = dmx*dmx + dmy*dmy in
                if dmr2 > 0.000001 then (
                    let s = 1. / dmr2 in
                    let scale = if s > 600. then 600. else s in
                    Point.set_dmx data p1 (dmx*scale);
                    Point.set_dmy data p1 (dmy*scale);
                );

                (* Clear flags, keep corner *)
                Point.set_flag flags p1 (
                    if Point.has_flag flags p1 PointFlags.corner
                    then PointFlags.corner
                    else PointFlags.no_flags
                  );

                (* Keep track of left turns *)
                let dx1 = Point.get_dx data p1 in
                let dy1 = Point.get_dy data p1 in
                let dx0 = Point.get_dx data p0 in
                let dy0 = Point.get_dy data p0 in
                let cross = dx1*dy0 - dx0*dy1 in
                if cross > 0. then (
                    incr left;
                    Point.add_flag flags p1 PointFlags.left;
                );

                (* Calculate if we should use bevel or miter for inner join *)
                let len0 = Point.len data p0 in
                let len1 = Point.len data p1 in
                let limit = max 1.01 ((min len0 len1) * iw) in
                if (dmr2 * limit*limit) < 1. then (
                    Point.add_flag flags p1 PointFlags.inner_bevel;
                );

                (* Check to see if the corner needs to be beveled *)
                if Point.has_flag flags  p1 PointFlags.corner then (
                    if dmr2*miter_limit*miter_limit < 1.
                        || LineJoin.equal line_join LineJoin.Bevel
                        || LineJoin.equal line_join LineJoin.Round
                    then (
                        Point.add_flag flags p1 PointFlags.bevel;
                    )
                );

                if Point.has_flag flags p1 PointFlags.bevel
                    || Point.has_flag flags p1 PointFlags.inner_bevel
                then (
                    path.nbevel <- path.nbevel +. 1;
                );

                p0_off := !p1_off;
                incr p1_off;
            done;

            path.convex <- !left =. path.count;
          done
    ;;

    let choose_bevel bevel (data : Point.data) (p0 : int) (p1 : int) w =
        let p1x = Point.get_x data p1 in
        let p1y = Point.get_y data p1 in
        if bevel then (
            let p1dx = Point.get_dx data p1 in
            let p1dy = Point.get_dy data p1 in
            let p0dx = Point.get_dx data p0 in
            let p0dy = Point.get_dy data p0 in
            let x0 = p1x +. p0dy*.w in
            let y0 = p1y -. p0dx*.w in
            let x1 = p1x +. p1dy*.w in
            let y1 = p1y -. p1dx*.w in
            x0, y0, x1, y1
        ) else (
            let p1dmx = Point.get_dmx data p1 in
            let p1dmy = Point.get_dmy data p1 in
            let x0 = p1x +. p1dmx*.w in
            let y0 = p1y +. p1dmy*.w in
            let x1 = p1x +. p1dmx*.w in
            let y1 = p1y +. p1dmy*.w in
            x0, y0, x1, y1
        )
    ;;

    let bevel_join verts offset (points : Point.t) (p0 : int) (p1 : int) lw rw lu ru =
        let data = points.data in
        let flags = points.flags in
        let offset = ref offset in
        let [@inline always] set x y u v =
            VertexBuffer.set verts !offset x y u v;
            incr offset;
        in
        let dlx0 = Point.get_dy data p0 in
        let dly0 = ~-.(Point.get_dx data p0) in
        let dlx1 = Point.get_dy data p1 in
        let dly1 = ~-.(Point.get_dx data p1) in
        let p1x = Point.get_x data p1 in
        let p1y = Point.get_y data p1 in
        let inner = Point.has_flag flags p1 PointFlags.inner_bevel in
        if Point.has_flag flags p1 PointFlags.left then (
            let lx0, ly0, lx1, ly1 = choose_bevel inner data p0 p1 lw in
            set lx0 ly0 lu 1.;
            set (p1x -. dlx0*.rw) (p1y -. dly0*.rw) ru 1.;

            if Point.has_flag flags p1 PointFlags.bevel then (
                set lx0 ly0 lu 1.;
                set (p1x -. dlx0*.rw) (p1y -. dly0*.rw) ru 1.;

                set lx1 ly1 lu 1.;
                set (p1x -. dlx1*.rw) (p1y -. dly1*.rw) ru 1.;
            ) else (
                let rx0 = p1x -. (Point.get_dmx data p1)*.rw in
                let ry0 = p1y -. (Point.get_dmy data p1)*.rw in

                set p1x p1y 0.5 1.;
                set (p1x -. dlx0*.rw) (p1y -. dly0*.rw) ru 1.;

                set rx0 ry0 ru 1.;
                set rx0 ry0 ru 1.;

                set p1x p1y 0.5 1.;
                set (p1x -. dlx1*.rw) (p1y -. dly1*.rw) ru 1.;
            );

            set lx1 ly1 lu 1.;
            set (p1x -. dlx1*.rw) (p1y -. dly1*.rw) ru 1.;
        ) else (
            let rx0, ry0, rx1, ry1 = choose_bevel inner data p0 p1 ~-.rw in

            set (p1x +. dlx0*.lw) (p1y +. dly0*.lw) lu 1.;
            set rx0 ry0 ru 1.;

            if Point.has_flag flags p1 PointFlags.bevel then (
                set (p1x +. dlx0*.lw) (p1y +. dly0*.lw) lu 1.;
                set rx0 ry0 ru 1.;

                set (p1x +. dlx1*.lw) (p1y +. dly1*.lw) lu 1.;
                set rx1 ry1 ru 1.;
            ) else (
                let lx0 = p1x +. (Point.get_dmx data p1)*.lw in
                let ly0 = p1y +. (Point.get_dmy data p1)*.lw in

                set (p1x +. dlx0*.lw) (p1y +. dly0*.lw) lu 1.;
                set p1x p1y 0.5 1.;

                set lx0 ly0 lu 1.;
                set lx0 ly0 lu 1.;

                set (p1x +. dlx1*.lw) (p1y +. dly1*.lw) lu 1.;
                set p1x p1y 0.5 1.;
            );

            set (p1x +. dlx1*.lw) (p1y +. dly1*.lw) lu 1.;
            set rx1 ry1 ru 1.;
        );
        !offset
    ;;

    let expand_fill_aa (t : t) =
        let open FloatOps in
        let aa = t.fringe_width in
        let w = aa in

        let convex =
            DynArray.length t.cache.paths =. 1
            && DynArray.(first t.cache.paths).convex
        in

        let points = t.cache.points in
        let flags = points.flags in
        let data = points.data in
        let woff = 0.5*aa in
        let verts = ref VertexBuffer.(num_verts t.cache.verts) in
        let dst = ref !verts in
        for i=0 to DynArray.length t.cache.paths -. 1 do
            let path = DynArray.get t.cache.paths i in
            dst := !verts;
            let [@inline always] get_pt idx =
                (path.first +. idx)
            in
            let p0 = get_pt (path.count-.1) |> ref in
            let p1_off = ref 0 in

            for _=0 to path.count-.1 do
                let p1 = get_pt (!p1_off) in
                if Point.has_flag flags p1 PointFlags.bevel then (
                    if Point.has_flag flags p1 PointFlags.left then (
                        let lx = (Point.get_x data p1) + (Point.get_dmx data p1)*woff in
                        let ly = (Point.get_y data p1) + (Point.get_dmy data p1)*woff in
                        VertexBuffer.set t.cache.verts !dst lx ly 0.5 1.;
                        incr dst;
                    ) else (
                        let dy0 = Point.get_dy  data !p0 in
                        let dlx0 = dy0 in
                        let dly0 = ~-.(dy0) in
                        let dlx1 = (Point.get_dy data p1) in
                        let dly1 = ~-.(Point.get_dx data p1) in
                        let p1x = Point.get_x data p1 in
                        let p1y = Point.get_y data p1 in
                        let lx0 = p1x + dlx0*woff in
                        let ly0 = p1y + dly0*woff in
                        let lx1 = p1x + dlx1*woff in
                        let ly1 = p1y + dly1*woff in
                        VertexBuffer.check_size t.cache.verts (!dst+.1);
                        (VertexBuffer.unsafe_set[@cold]) t.cache.verts !dst lx0 ly0 0.5 1.;
                        (VertexBuffer.unsafe_set[@cold]) t.cache.verts (!dst+.1) lx1 ly1 0.5 1.;
                        dst := !dst +. 2;
                    )
                ) else (
                    (* these are right *)
                    let x = (Point.get_x data p1) + (Point.get_dmx data p1)*woff in
                    let y = (Point.get_y data p1) + (Point.get_dmy data p1)*woff in
                    VertexBuffer.set t.cache.verts !dst x y 0.5 1.;
                    incr dst;
                );
                p0 := p1;
                incr p1_off;
            done;

            let nfill = (!dst -. !verts) in
            path.fill <- VertexBuffer.Sub.sub t.cache.verts !verts nfill;
            verts := !dst;

            let flag =
                PointFlags.no_flags
                |> PointFlags.add ~flag:PointFlags.bevel
                |> PointFlags.add ~flag:PointFlags.inner_bevel
            in

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

            let [@inlined] p1_off = ref 0 in
            let [@inlined] p0 = ref (get_pt (path.count -. 1)) in
            for _=0 to path.count-.1 do
                let [@inlined] p1 = get_pt !p1_off in
                if Point.has_flag flags p1 flag then (
                    dst := bevel_join t.cache.verts !dst t.cache.points !p0 p1 lw rw lu ru
                ) else (
                    VertexBuffer.check_size t.cache.verts (!dst +. 2);
                    let p1x = Point.get_x data p1 in
                    let p1y = Point.get_y data p1 in
                    let p1dmx = Point.get_dmx data p1 in
                    let p1dmy = Point.get_dmy data p1 in
                    let [@inlined] x = p1x + p1dmx*lw in
                    let [@inlined] y = p1y + p1dmy*lw in
                    VertexBuffer.unsafe_set t.cache.verts !dst x y lu 1.;
                    let [@inlined] x = p1x - p1dmx*rw in
                    let [@inlined] y = p1y - p1dmy*rw in
                    VertexBuffer.unsafe_set t.cache.verts (!dst+.1) x y ru 1.;
                    dst := !dst +. 2;
                );
                p0 := p1;
                incr p1_off;
            done;

            (* Loop it *)
            VertexBuffer.check_size t.cache.verts (!dst +. 2);
            let [@inlined] v0_x, v0_y, _, _ = VertexBuffer.get t.cache.verts !verts in
            VertexBuffer.unsafe_set t.cache.verts !dst v0_x v0_y lu 1.;
            let [@inlined] v1_x, v1_y, _, _ = VertexBuffer.get t.cache.verts (!verts +. 1) in
            VertexBuffer.unsafe_set t.cache.verts (!dst+.1) v1_x v1_y ru 1.;
            dst := !dst +. 2;

            let nstroke = !dst -. !verts in
            assert (nstroke >. 0);
            path.stroke <- VertexBuffer.Sub.sub t.cache.verts !verts nstroke;

            verts := !dst
          done
    ;;

    let expand_fill_no_aa (t : t) =
        let open FloatOps in

        let points = t.cache.points in
        let verts = ref VertexBuffer.(num_verts t.cache.verts) in
        let dst = ref !verts in
        for i=0 to DynArray.length t.cache.paths -. 1 do
            let path = DynArray.get t.cache.paths i in
            dst := !verts;
            let [@inline always] get_pt idx =
                (path.first +. idx)
            in

            VertexBuffer.check_size t.cache.verts (!dst +. path.count);
            for j=0 to path.count-.1 do
                let point = get_pt j in
                let x, y = Point.get_xy points point in
                VertexBuffer.unsafe_set t.cache.verts (!dst+.j) x y 0.5 1.;
            done;
            dst := !dst +. path.count;

            let nfill = (!dst -. !verts) in
            path.fill <- VertexBuffer.Sub.sub t.cache.verts !verts nfill;
            verts := !dst;

            path.stroke <- VertexBuffer.Sub.empty;
          done
    ;;

    let curve_divs r arc tol =
        let open FloatOps in
        let da = (Float.acos (r / (r + tol))) * 2. in
        imax 2 (int_of_float (Float.ceil (arc / da)))
    ;;

    let butt_cap_start verts dst px py dx dy w d aa u0 u1 =
        let after = dst + 4 in
        let open FloatOps in
        let px = px - dx*d in
        let py = py - dy*d in
        let dlx = dy in
        let dly = ~-.dx in
        VertexBuffer.check_size verts (dst+.3);
        VertexBuffer.unsafe_set verts (dst+.0) (px + dlx*w - dx*aa) (py + dly*w - dy*aa) u0 0.;
        VertexBuffer.unsafe_set verts (dst+.1) (px - dlx*w - dx*aa) (py - dly*w - dy*aa) u1 0.;
        VertexBuffer.unsafe_set verts (dst+.2) (px + dlx*w) (py + dly*w) u0 1.;
        VertexBuffer.unsafe_set verts (dst+.3) (px - dlx*w) (py - dly*w) u1 1.;
        after
    ;;

    let butt_cap_end verts dst px py dx dy w d aa u0 u1 =
        let after = dst + 4 in
        let open FloatOps in
        let px = px + dx*d in
        let py = py + dy*d in
        let dlx = dy in
        let dly = ~-.dx in
        VertexBuffer.check_size verts (dst+.3);
        VertexBuffer.unsafe_set verts (dst+.0) (px + dlx*w) (py + dly*w) u0 1.;
        VertexBuffer.unsafe_set verts (dst+.1) (px - dlx*w) (py - dly*w) u1 1.;
        VertexBuffer.unsafe_set verts (dst+.2) (px + dlx*w + dx*aa) (py + dly*w + dy*aa) u0 0.;
        VertexBuffer.unsafe_set verts (dst+.3) (px - dlx*w + dx*aa) (py - dly*w + dy*aa) u1 0.;
        after
    ;;

    let round_cap_start verts dst px py dx dy w ncap u0 u1 =
        let open FloatOps in
        let px = px in
        let py = py in
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

    let round_cap_end verts dst px py dx dy w ncap u0 u1 =
        let i = ref 0 in
        let open FloatOps in
        let px = px in
        let py = py in
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

    let round_join verts dst (points : Point.t) (p0 : int) (p1 : int) lw rw lu ru ncap =
        let flags = points.flags in
        let data = points.data in
        let dlx0 = Point.get_dy data p0 in
        let dly0 = ~-.(Point.get_dx data p0) in
        let dlx1 = Point.get_dy data p1 in
        let dly1 = ~-.(Point.get_dx data p1) in
        if Point.has_flag flags p1 PointFlags.left then (
            let inner = Point.has_flag flags p1 PointFlags.inner_bevel in
            let lx0, ly0, lx1, ly1 = choose_bevel inner data p0 p1 lw in
            let a0 = Float.atan2 ~-.dly0 ~-.dlx0 in
            let a1 = Float.atan2 ~-.dly1 ~-.dlx1 in
            let a1 = if a1 > a0 then a1 -. Float.pi*.2. else a1 in

            let p1x = Point.get_x data p1 in
            let p1y = Point.get_y data p1 in
            VertexBuffer.set verts !dst lx0 ly0 lu 1.;
            incr dst;
            VertexBuffer.set verts !dst (p1x -. dlx0*.rw) (p1y -. dly0*.rw) ru 1.;
            incr dst;

            let n = clamp
                Float.(ceil(((a0 -. a1) /. pi) *. float ncap) |> int_of_float)
                2
                ncap
            in
            for i=0 to n-1 do
                let u = float i /. (float n -. 1.) in
                let a = a0 +. u*.(a1 -. a0) in
                let rx = p1x +. Float.cos a *. rw in
                let ry = p1y +. Float.sin a *. rw in
                VertexBuffer.set verts !dst p1x p1y 0.5 1.;
                incr dst;
                VertexBuffer.set verts !dst rx ry ru 1.;
                incr dst;
            done;

            VertexBuffer.set verts !dst lx1 ly1 lu 1.;
            incr dst;
            VertexBuffer.set verts !dst (p1x -. dlx1*.rw) (p1y -. dly1*.rw) ru 1.;
            incr dst;
        ) else (
            let inner = Point.has_flag flags p1 PointFlags.inner_bevel in
            let rx0, ry0, rx1, ry1 = choose_bevel inner data p0 p1 ~-.rw in
            let a0 = Float.atan2 dly0 dlx0 in
            let a1 = Float.atan2 dly1 dlx1 in
            let a1 = if a1 < a0 then a1 +. Float.pi*.2. else a1 in

            let p1x = Point.get_x data p1 in
            let p1y = Point.get_y data p1 in
            VertexBuffer.set verts !dst (p1x +. dlx0*.rw) (p1y +. dly0*.rw) lu 1.;
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
                let lx = p1x +. Float.cos a *. lw in
                let ly = p1y +. Float.sin a *. lw in
                VertexBuffer.set verts !dst lx ly lu 1.;
                incr dst;
                VertexBuffer.set verts !dst p1x p1y 0.5 1.;
                incr dst;
            done;

            VertexBuffer.set verts !dst (p1x +. dlx1*.rw) (p1y +. dly1*.rw) lu 1.;
            incr dst;
            VertexBuffer.set verts !dst rx1 ry1 ru 1.;
            incr dst;
        )
    ;;

    let expand_stroke t w fringe line_cap line_join miter_limit =
        let open FloatOps in
        let aa = fringe in

        let ncap = curve_divs w Float.pi t.tess_tol in

        let w = w + aa * 0.5 in

        let u0, u1 =
            if aa = 0. then (
                0.5, 0.5
            ) else (
                0., 1.
            )
        in

        calculate_joins t w line_join miter_limit;

        let verts = ref (VertexBuffer.num_verts t.cache.verts) in
        let data = t.cache.points.data in
        let flags = t.cache.points.flags in
        let dst = ref !verts in
        for i=0 to DynArray.length t.cache.paths -. 1 do
            let path = DynArray.get t.cache.paths i in
            let get idx =
                (path.first +. idx)
            in

            path.fill <- VertexBuffer.Sub.empty;

            dst := !verts;

            let s = ref 0 in
            let e = ref path.count in
            let p0_off = ref (path.count-.1) in
            let p1_off = ref 0 in

            if not path.closed then (
                p0_off := 0;
                p1_off := 1;
                s := 1;
                e := path.count-.1;

                let p0 = get !p0_off in
                let p1 = get !p1_off in
                let dx = (Point.get_x data p1) - (Point.get_x data p0) in
                let dy = (Point.get_y data p1) - (Point.get_y data p0) in
                let _, dx, dy = Point.normalize dx dy in
                let p0x = Point.get_x data p0 in
                let p0y = Point.get_y data p0 in
                match line_cap with
                | LineCap.Butt -> dst := butt_cap_start t.cache.verts !dst p0x p0y dx dy w (~-.aa*0.5) aa u0 u1
                | Square -> dst := butt_cap_start t.cache.verts !dst p0x p0y dx dy w (w - aa) aa u0 u1
                | Round -> round_cap_start t.cache.verts dst p0x p0y dx dy w ncap u0 u1
                | Default -> ()
            );

            let j = ref !s in
            while !j <. !e do
                let p0 = get !p0_off in
                let p1 = get !p1_off in
                if Point.has_flag flags p1 PointFlags.bevel
                    || Point.has_flag flags p1 PointFlags.inner_bevel then (
                        if LineJoin.equal line_join LineJoin.Round then (
                            round_join t.cache.verts dst t.cache.points p0 p1 w w u0 u1 ncap
                        ) else (
                            dst := bevel_join t.cache.verts !dst t.cache.points p0 p1 w w u0 u1
                        );
                ) else (
                    let p1x = Point.get_x data p1 in
                    let p1y = Point.get_y data p1 in
                    let p1dmx = Point.get_dmx data p1 in
                    let p1dmy = Point.get_dmy data p1 in
                    VertexBuffer.set t.cache.verts !dst (p1x + (p1dmx*w)) (p1y + (p1dmy*w)) u0 1.;
                    incr dst;
                    VertexBuffer.set t.cache.verts !dst (p1x - (p1dmx*w)) (p1y - (p1dmy*w)) u1 1.;
                    incr dst;
                );

                p0_off := !p1_off;
                incr p1_off;
                incr j
            done;

            if path.closed then (
                let v0x, v0y, _, _ = VertexBuffer.get t.cache.verts !verts in
                let v1x, v1y, _, _ = VertexBuffer.get t.cache.verts (!verts+.1) in
                VertexBuffer.set t.cache.verts !dst v0x v0y u0 1.;
                incr dst;
                VertexBuffer.set t.cache.verts !dst v1x v1y u1 1.;
                incr dst;
            ) else (
                let p0 = get !p0_off in
                let p1 = get !p1_off in
                (* add line cap *)
                let dx = (Point.get_x data p1) - (Point.get_x data p0) in
                let dy = (Point.get_y data p1) - (Point.get_y data p0) in
                let _, dx, dy = Point.normalize dx dy in
                let p1x = Point.get_x data p1 in
                let p1y = Point.get_y data p1 in
                match line_cap with
                | Butt -> dst := butt_cap_end t.cache.verts !dst p1x p1y dx dy w (~-.aa*0.5) aa u0 u1
                | Square -> dst := butt_cap_end t.cache.verts !dst p1x p1y dx dy w (w-aa) aa u0 u1
                | Round -> round_cap_end t.cache.verts dst p1x p1y dx dy w ncap u0 u1
                | Default -> ()
            );

            let len = !dst -. !verts in
            path.stroke <- VertexBuffer.Sub.sub t.cache.verts !verts len;

            verts := !dst;
          done
    ;;

    let stroke_calc_width (t : t) (state : State.t) =
        let scale = Matrix.get_average_scale state.xform in
        let stroke_width = clamp (state.stroke_width *. scale) 0. 200. |> ref in
        let before = !stroke_width in

        if !stroke_width < t.fringe_width then (
            stroke_width := t.fringe_width;
        );
        before, !stroke_width
    ;;

    let stroke_prepare (t : t) (state : State.t) =
        let before, stroke_width = stroke_calc_width t state in

        Path.flatten t;

        if Impl.edge_antialias t.impl && state.shape_anti_alias then (
            expand_stroke t (stroke_width*.0.5) t.fringe_width state.line_cap state.line_join state.miter_limit;
        ) else (
            expand_stroke t (stroke_width*.0.5) 0.0 state.line_cap state.line_join state.miter_limit
        );

        before
    ;;

    let stroke_finish (t : t) (state : State.t) stroke_width =
        let stroke_paint = Paint.copy state.stroke in
        let stroke_width = ref stroke_width in
        if !stroke_width < t.fringe_width then (
            let alpha = clamp (!stroke_width /. t.fringe_width) 0. 1. in
            let alpha = alpha*.alpha in
            Paint.modify_alpha stroke_paint alpha;
            stroke_width := t.fringe_width;
        );

        (* Apply global alpha *)
        Paint.modify_alpha stroke_paint state.alpha;

        Impl.stroke t.impl
            ~paint:stroke_paint
            ~composite_op:state.composite_operation
            ~scissor:state.scissor
            ~fringe:t.fringe_width
            ~stroke_width:!stroke_width
            ~paths:t.cache.paths;
    ;;

    let stroke (t: t) =
        let state = get_state t in
        let stroke_width = stroke_prepare t state in
        stroke_finish t state stroke_width
    ;;

    let fill_prepare (t : t) (state : State.t) =
        Path.flatten t;

        if Impl.edge_antialias t.impl && state.shape_anti_alias then (
            calculate_joins t t.fringe_width LineJoin.Miter 2.4;
            expand_fill_aa t;
        ) else (
            calculate_joins t 0. LineJoin.Miter 2.4;
            expand_fill_no_aa t;
        );
    ;;

    let fill_finish (t : t) (state : State.t) =
        let fill_paint = Paint.copy state.fill in
        Paint.modify_alpha fill_paint state.alpha;

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

    let fill (t : t) =
        let state = get_state t in
        fill_prepare t state;
        fill_finish t state;
    ;;

    module Cache = struct
        type kind = Stroke
                  | Fill
                  | Fill_stroke
                  | Stroke_fill

        type path = {
            fill_offset : int;
            fill_length : int;
            stroke_offset : int;
            stroke_length : int;
        }

        let empty_path = {
            fill_offset = 0;
            fill_length = 0;
            stroke_offset = 0;
            stroke_length = 0;
        }

        type cached = {
            values : Buffer.Float.t;
            paths : path array;
            kind : kind;
            stroke_width : float;
            fringe_width : float;
        }

        let begin_ = Path.begin_

        let draw t tess ~x:tx ~y:ty =
            (* Dump the state into the vertex buffer *)
            let open FloatOps in
            let state = get_state t in
            let m = state.xform in
            let i = ref 0 in
            let len = Buffer.Float.length tess.values /. 4 in
            let start = VertexBuffer.num_verts t.cache.verts in
            VertexBuffer.set t.cache.verts (start+.len) 0. 0. 0. 0.;
            let vbuff = VertexBuffer.unsafe_array t.cache.verts in
            let off = ref (start*.4) in

            let xmin = ref 1e6 in
            let ymin = ref 1e6 in
            let xmax = ref ~-.1e6 in
            let ymax = ref ~-.1e6 in

            while !i <. len*.4 do
                let x = Buffer.Float.get tess.values !i in
                let y = Buffer.Float.get tess.values (!i+.1) in
                let u = Buffer.Float.get tess.values (!i+.2) in
                let v = Buffer.Float.get tess.values (!i+.3) in

                let x = x*m.m0 + y*m.m2 + m.m4 + tx in
                let y = x*m.m1 + y*m.m3 + m.m5 + ty in

                xmin := min !xmin x;
                ymin := min !ymin y;
                xmax := max !xmax x;
                ymax := max !ymax y;

                Buffer.Float.set vbuff (!off) x;
                Buffer.Float.set vbuff (!off+.1) y;

                Buffer.Float.set vbuff (!off+.2) u;
                Buffer.Float.set vbuff (!off+.3) v;
                off := !off +. 4;
                i := !i +. 4;
            done;

            let old_width = state.stroke_width in
            let old_fringe = t.fringe_width in
            let old_bounds = t.cache.bounds in
            state.stroke_width <- tess.stroke_width;
            t.fringe_width <- tess.fringe_width;
            t.cache.bounds <- Bounds.{
                xmin = !xmin;
                ymin = !ymin;
                xmax = !xmax;
                ymax = !ymax;
            };
            let _, width = stroke_calc_width t state in

            Array.iter (fun p ->
                add_path t;
                let path = last_path t in
                path.fill <- VertexBuffer.Sub.sub t.cache.verts (start +. p.fill_offset) p.fill_length;
                path.stroke <- VertexBuffer.Sub.sub t.cache.verts (start +. p.stroke_offset) p.stroke_length;
            ) tess.paths;

            begin match tess.kind with
            | Stroke ->
                stroke_finish t state width;
            | Fill ->
                fill_finish t state;
            | Fill_stroke ->
                fill_finish t state;
                stroke_finish t state width;
            | Stroke_fill ->
                stroke_finish t state width;
                fill_finish t state;
            end;
            state.stroke_width <- old_width;
            t.fringe_width <- old_fringe;
            t.cache.bounds <- old_bounds;
        ;;

        let save_preamble t kind (state : State.t) =
            begin match kind with
            | Stroke -> stroke_prepare t state |> ignore;
            | Fill -> fill_prepare t state;
            | Stroke_fill ->
                stroke_prepare t state |> ignore;

                let copy = DynArray.copy t.cache.paths IPath.copy in
                let temp = t.cache.paths in
                t.cache.paths <- copy;

                fill_prepare t state;

                DynArray.append_steal ~dst:temp ~src:copy IPath.create;
                t.cache.paths <- temp;
            | Fill_stroke ->
                fill_prepare t state;

                let copy = DynArray.copy t.cache.paths IPath.copy in
                let temp = t.cache.paths in
                t.cache.paths <- copy;

                stroke_prepare t state |> ignore;

                DynArray.append_steal ~dst:temp ~src:copy IPath.create;
                t.cache.paths <- temp;
            end;
        ;;

        let total_path_length (paths : IPath.t DynArray.t) =
            let length = ref 0 in
            for i=0 to DynArray.length paths - 1 do
                let path = DynArray.get paths i in
                length := !length + VertexBuffer.Sub.num_verts path.fill;
                length := !length + VertexBuffer.Sub.num_verts path.stroke;
            done;
            !length
        ;;

        let save t kind =
            let state = get_state t in
            save_preamble t kind state;

            (* Save off all the paths *)
            let length = total_path_length t.cache.paths in
            let buffer = Buffer.Float.create (length*4) in
            let paths = Array.make DynArray.(length t.cache.paths) empty_path in

            let i = ref 0 in
            let off = ref 0 in
            let vbuff = VertexBuffer.unsafe_array t.cache.verts in

            let l = DynArray.length t.cache.paths - 1 in
            for idx=0 to l do
                let path = DynArray.get t.cache.paths idx in
                let fill_length = VertexBuffer.Sub.num_verts path.fill in
                let stroke_length = VertexBuffer.Sub.num_verts path.stroke in

                let fill_off = VertexBuffer.Sub.vertex_offset path.fill in
                let stroke_off = VertexBuffer.Sub.vertex_offset path.stroke in
                Buffer.Float.blit ~src:vbuff ~dst:buffer ~s_off:(fill_off*4) ~d_off:(!off*4) ~len:(fill_length*4);
                Buffer.Float.blit ~src:vbuff ~dst:buffer ~s_off:(stroke_off*4) ~d_off:((!off+fill_length)*4) ~len:(stroke_length*4);

                let path = {
                    fill_offset = !off;
                    fill_length;
                    stroke_offset = !off + fill_length;
                    stroke_length;
                } in
                off := !off + fill_length + stroke_length;
                paths.(!i) <- path;
                incr i;
              done;

            {
                values = buffer;
                kind;
                paths;
                stroke_width = state.stroke_width;
                fringe_width = t.fringe_width;
            }
        ;;
    end

    let end_frame t =
        Impl.flush t.impl t.cache.verts;

        if t.font_image_idx <> 0 then (
            (* TODO - add font/text stuff *)
        );
    ;;

    let set_paint_color (p : Paint.t) (color : Color.t) : unit =
        Paint.change_color_keep_extent p color
    ;;

    module Paint = struct
        type ctx = t
        type t = Graphv_core_lib.Paint.t

        let linear_gradient (_t : ctx) ~sx ~sy ~ex ~ey ~icol ~ocol : t =
            let open FloatOps in
            let paint = Paint.create() in
            (*Matrix.identity paint.xform;*)
            Paint.reset_xform paint;
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

            let large = 1e5 in
            paint.m0 <- dy;
            paint.m1 <- ~-.dx;
            paint.m2 <- dx;
            paint.m3 <- dy;
            paint.m4 <- (sx - dx*large);
            paint.m5 <- (sy - dy*large);

            paint.extent_x <- large;
            paint.extent_y <- large + d*0.5;
            paint.radius <- 0.;
            paint.feather <- max 1. d;
            Paint.set_only_inner_and_outer paint icol ocol;
            paint
        ;;

        let box_gradient (_t : ctx) ~x ~y ~w ~h ~r ~f ~icol ~ocol : t =
            let open FloatOps in
            let paint = Paint.create() in
            Paint.reset_xform paint;
            paint.m4 <- x+w*0.5;
            paint.m5 <- y+h*0.5;

            paint.extent_x <- w*0.5;
            paint.extent_y <- h*0.5;
            paint.radius <- r;
            paint.feather <- max 1. f;
            Paint.set_only_inner_and_outer paint icol ocol;
            paint
        ;;

        let radial_gradient (_t : ctx) ~cx ~cy ~in_radius ~out_radius ~icol ~ocol : t =
            let open FloatOps in
            let paint = Paint.create() in
            Paint.reset_xform paint;
            paint.m4 <- cx;
            paint.m5 <- cy;

            let r = (in_radius + out_radius)*0.5 in
            let f = (out_radius - in_radius) in

            paint.extent_x <- r;
            paint.extent_y <- r;
            paint.radius <- r;
            paint.feather <- max 1. f;
            Paint.set_only_inner_and_outer paint icol ocol;
            paint
        ;;

        let image_pattern (_t : ctx) ~cx ~cy ~w ~h ~angle ~image ~alpha : t =
            let paint = Paint.create() in
            Paint.rotate paint angle;

            paint.m4 <- cx;
            paint.m5 <- cy;

            paint.extent_x <- w; 
            paint.extent_y <- h;
            paint.image <- float image;

            Paint.reset_colors_with_alpha paint alpha;

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
        Graphv_core_lib.Paint.multiply state.fill state.xform;
    ;;

    let set_stroke_color t ~color =
        let state = get_state t in
        set_paint_color state.stroke color
    ;;

    let set_stroke_paint t ~paint =
        let state = get_state t in
        state.stroke <- Graphv_core_lib.Paint.copy paint;
        Graphv_core_lib.Paint.multiply state.stroke state.xform;
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

        paint.image <- float (DynArray.get t.font_images t.font_image_idx);

        Graphv_core_lib.Paint.modify_alpha paint state.alpha;

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
            let at = VertexBuffer.num_verts verts in
            VertexBuffer.check_size verts (at+5);

            let open FloatOps in
            (* Transform corners *)
            let qx0 = q.x0*inv_scale in
            let qy0 = q.y0*inv_scale in
            let qx1 = q.x1*inv_scale in
            let qy1 = q.y1*inv_scale in

            let c0, c1 = (Matrix.transform_point[@inlined]) xm qx0 qy0 in
            let c4, c5 = (Matrix.transform_point[@inlined]) xm qx1 qy1 in
            let c2, c3 = (Matrix.transform_point[@inlined]) xm qx1 qy0 in
            let c6, c7 = (Matrix.transform_point[@inlined]) xm qx0 qy1 in

            VertexBuffer.unsafe_set verts at c0 c1 q.s0 q.t0;
            VertexBuffer.unsafe_set verts (at+.1) c4 c5 q.s1 q.t1;
            VertexBuffer.unsafe_set verts (at+.2) c2 c3 q.s1 q.t0;
            VertexBuffer.unsafe_set verts (at+.3) c0 c1 q.s0 q.t0;
            VertexBuffer.unsafe_set verts (at+.4) c6 c7 q.s0 q.t1;
            VertexBuffer.unsafe_set verts (at+.5) c4 c5 q.s1 q.t1;
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
                line_height = metrics.line_height /. scale;
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

        let codepoint_equal a b =
            match a, b with
            | Space, Space
            | Newline, Newline
            | Char, Char
            | CJK_char, CJK_char -> true
            | _ -> false
        ;;

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

            let ( = ) = codepoint_equal in

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
                                if (type_ = Char || type_ = CJK_char) then (
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
                                if (ptype = Space && (type_ = Char || type_ = CJK_char))
                                    || type_ = CJK_char
                                then
                                (
                                    word_start := FI.start iter;
                                    word_start_x := FI.x iter;
                                    word_min_x := quad.x0;
                                );

                                (* Track last non-whitespace character *)
                                if type_ = Char || type_ = CJK_char then (
                                    row_end := FI.next iter;
                                    row_width := FI.next_x iter - !row_start_x;
                                    row_max_x := quad.x1 - !row_start_x;
                                );

                                (* Track last end of word *)
                                if ((ptype = Char || ptype = CJK_char) && type_ = Space)
                                    || (type_ = CJK_char) then
                                (
                                    break_end := FI.next iter;
                                    word_start := FI.next iter;
                                    break_width := !row_width;
                                    break_max_x := !row_max_x;
                                );

                                (* Break to new line when a character is beyond break width *)
                                if
                                    (type_ = Char || type_ = CJK_char) && next_width > break_row_width
                                then (

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
                        min_x = (min (F.Iter.x iter) quad.x0) * inv_scale;
                        max_x = (max (F.Iter.next_x iter) quad.x1) * inv_scale;
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
            let ymin, ymax = FontContext.line_bounds t.fs (y *.scale) in

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
                        min_x := min !min_x rmin_x;
                        max_x := max !max_x rmax_x;
                        min_y := min !min_y (!y + rmin_y);
                        max_y := max !max_y (!y + rmax_y);

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
            tesselate_afd = CreateFlags.has ~flag:CreateFlags.tesselate_afd flags;
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
