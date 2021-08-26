let clampf a mn mx =
    if a < mn then mn
    else if a > mx then mx
    else a
;;

let is_black (c : NVG.Color.t) =
    c.r = 0.0 
    && c.g = 0.0 
    && c.b = 0.0 
    && c.a = 1.0
;;

let to_utf8 (c : int) =
    let n =
        if c < 0x80 then 1
        else if c < 0x800 then 2
        else if c < 0x10000 then 3
        else if c < 0x200000 then 4
        else if c < 0x4000000 then 5
        else if c <= 0x7fffffff then 6
        else 6
    in
    let seq = Bytes.create n in
    let ( .%()<- ) b i v = Bytes.set b i (char_of_int v) in
    let rec loop n c =
        match n with
        | 6 -> seq.%(5) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x4000000)
        | 5 -> seq.%(4) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x200000)
        | 4 -> seq.%(3) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x10000)
        | 3 -> seq.%(2) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x800)
        | 2 -> seq.%(1) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0xc0)
        | 1 -> seq.%(0) <- c
        | _ -> ()
    in
    loop n c;
    Bytes.to_string seq
;;

let draw_window (vg : NVG.t) (title : string) x y w h =
    let open NVG in
    let open FloatOps in

    let corner_radius = 3.0 in

    save vg;

    (* Window *)
    Path.begin_ vg;
    Path.rounded_rect vg ~x ~y ~w ~h ~r:corner_radius;
    set_fill_color vg ~color:Color.(rgba ~r:28 ~g:30 ~b:34 ~a:192);
    fill vg;

    (* Drop shadow *)
    let shadow = Paint.box_gradient vg ~x ~y:(y+2.) ~w ~h ~r:(corner_radius*2.) ~f:10. 
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128)
        ~ocol:Color.transparent
    in
    Path.begin_ vg;
    Path.rect vg ~x:(x-10.) ~y:(y-10.) ~w:(w+20.) ~h:(h+30.);
    Path.rounded_rect vg ~x ~y ~w ~h ~r:corner_radius;
    Path.winding vg ~winding:Winding.CW;
    set_fill_paint vg ~paint:shadow;
    fill vg;

    (* Header *)
    let header = Paint.linear_gradient vg ~sx:x ~sy:y ~ex:x ~ey:(y+15.)
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:8)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:16)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+1.) ~y:(y+1.) ~w:(w-2.) ~h:30. ~r:(corner_radius-1.);
    set_fill_paint vg ~paint:header;
    fill vg;

    Path.begin_ vg;
    Path.move_to vg ~x:(x+0.5) ~y:(y+0.5+30.);
    Path.line_to vg ~x:(x+0.5+w-1.) ~y:(y+0.5+30.);
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32);
    stroke vg;

    Text.set_size vg ~size:15.;
    Text.set_font_face vg ~name:"sans-bold";
    Text.set_align vg ~align:Align.(center lor middle);
    Text.set_blur vg ~blur:2.;
    set_fill_color vg ~color:Color.(rgba ~r:0 ~g:0 ~b:0 ~a:128);
    Text.text vg ~x:(x+w/2.) ~y:(y+16.+1.) title;

    Text.set_blur vg ~blur:0.;
    set_fill_color vg ~color:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:160);
    Text.text vg ~x:(x+w/2.) ~y:(y+16.) title;

    restore vg;
;;

let draw_eyes (vg : NVG.t) x y w h mx my t =
    let open NVG in
    let open FloatOps in
    let ex = w * 0.23 in
    let ey = h * 0.5 in
    let lx = x + ex in
    let ly = y + ey in
    let rx = x + w - ex in
    let ry = y + ey in
    let br = (if ex < ey then ex else ey) * 0.5 in
    let blink = 1. - Float.(pow (sin (t*0.5)) 200.)*0.8 in

    let bg = Paint.linear_gradient vg ~sx:x ~sy:(y+h*0.5) ~ex:(x+w*0.1) ~ey:(y+h)
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:16)
    in
    Path.begin_ vg;
    Path.ellipse vg ~cx:(lx+3.) ~cy:(ly+16.) ~rx:ex ~ry:ey;
    Path.ellipse vg ~cx:(rx+3.) ~cy:(ry+16.) ~rx:ex ~ry:ey;
    set_fill_paint vg ~paint:bg;
    fill vg;

    let bg = Paint.linear_gradient vg ~sx:x ~sy:(y+h*0.25) ~ex:(x+w*0.1) ~ey:(y+h)
        ~icol:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:255)
        ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:255)
    in
    Path.begin_ vg;
    Path.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
    Path.ellipse vg ~cx:rx ~cy:ry ~rx:ex ~ry:ey;
    set_fill_paint vg ~paint:bg;
    fill vg;

    let dx = (mx - rx) / (ex * 10.) in
    let dy = (my - ry) / (ey * 10.) in
    let d = Float.sqrt (dx*dx + dy*dy) in
    let dx, dy =
        if d > 1. then dx / d, dy / d
        else dx, dy
    in
    let dx = dx*ex*0.4 in
    let dy = dy*ey*0.5 in
    Path.begin_ vg;
    Path.ellipse vg ~cx:(lx+dx) ~cy:(ly+dy+ey*0.25*(1.-blink)) ~rx:br ~ry:(br*blink);
    set_fill_color vg ~color:(Color.rgba ~r:32 ~g:32 ~b:32 ~a:255);
    fill vg;

    Path.begin_ vg;
    Path.ellipse vg ~cx:(rx+dx) ~cy:(ry+dy+ey*0.25*(1.-blink)) ~rx:br ~ry:(br*blink);
    set_fill_color vg ~color:(Color.rgba ~r:32 ~g:32 ~b:32 ~a:255);
    fill vg;

    let gloss = Paint.radial_gradient vg ~cx:(lx-ex*0.25) ~cy:(ly-ey*0.5)
        ~in_radius:(ex*0.1) ~out_radius:(ex*0.75)
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:128)
        ~ocol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:0)
    in
    Path.begin_ vg;
    Path.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
    set_fill_paint vg ~paint:gloss;
    fill vg;

    let gloss = Paint.radial_gradient vg ~cx:(rx-ex*0.25) ~cy:(ry-ey*0.5)
        ~in_radius:(ex*0.1) ~out_radius:(ex*0.75)
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:128)
        ~ocol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:0)
    in
    Path.begin_ vg;
    Path.ellipse vg ~cx:rx ~cy:ry ~rx:ex ~ry:ey;
    set_fill_paint vg ~paint:gloss;
    fill vg;
;;

let icon_search = 0x1F50D
let icon_circled_cross = 0x2716

let draw_search_box (vg : NVG.t) text x y w h =
    let open NVG in
    let open FloatOps in

    let corner_radius = h / 2. - 1. in

    let bg = Paint.box_gradient vg ~x ~y:(y+1.5) ~w ~h ~r:(h/2.) ~f:5. 
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:16)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x ~y ~w ~h ~r:corner_radius;
    set_fill_paint vg ~paint:bg;
    fill vg;

    Text.set_size vg ~size:(h*1.3);
    Text.set_font_face vg ~name:"icons";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:64);
    Text.set_align vg ~align:Align.(center lor middle);
    Text.text vg ~x:(x+h*0.55) ~y:(y+h*0.55) (to_utf8 icon_search);

    Text.set_size vg ~size:17.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:32);
    Text.set_align vg ~align:Align.(left lor middle);
    Text.text vg ~x:(x+h*1.05) ~y:(y+h*0.5) text;

    Text.set_size vg ~size:(h*1.3);
    Text.set_font_face vg ~name:"icons";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:32);
    Text.set_align vg ~align:Align.(center lor middle);
    Text.text vg ~x:(x+w-h*0.55) ~y:(y+h*0.55) (to_utf8 icon_circled_cross);
;;

let icon_chevron_right = 0xE75E

let draw_drop_down (vg : NVG.t) text x y w h =
    let open NVG in
    let open FloatOps in

    let corner_radius = 4. in

    let bg = Paint.linear_gradient vg ~sx:x ~sy:y ~ex:x ~ey:(y+h)
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:16)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:16)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+1.) ~y:(y+1.) ~w:(w-2.) ~h:(h-2.) ~r:(corner_radius-1.);
    set_fill_paint vg ~paint:bg;
    fill vg;

    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+0.5) ~y:(y+0.5) ~w:(w-1.) ~h:(h-1.) ~r:(corner_radius-0.5);
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:48);
    stroke vg;

    Text.set_size vg ~size:17.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:160);
    Text.set_align vg ~align:Align.(left lor middle);
    Text.text vg ~x:(x+h*0.3) ~y:(y+h*0.5) text;

    Text.set_size vg ~size:(h*1.3);
    Text.set_font_face vg ~name:"icons";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:64);
    Text.set_align vg ~align:Align.(center lor middle);
    Text.text vg ~x:(x+w-h*0.5) ~y:(y+h*0.5) (to_utf8 icon_chevron_right);
;;

let draw_label (vg : NVG.t) text x y _w h =
    let open NVG in
    let open FloatOps in
    Text.set_size vg ~size:15.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:128);

    Text.set_align vg ~align:Align.(left lor middle);
    Text.text vg ~x ~y:(y+h*0.5) text;
;;

let draw_edit_box_base (vg : NVG.t) x y w h =
    let open NVG in
    let open FloatOps in

    let bg = Paint.box_gradient vg ~x:(x+1.) ~y:(y+1.+1.5) ~w:(w-2.) ~h:(h-2.)
        ~r:3. ~f:4.
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:32)
        ~ocol:(Color.rgba ~r:32 ~g:32 ~b:32 ~a:32)
    in

    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+1.) ~y:(y+1.) ~w:(w-2.) ~h:(h-2.) ~r:3.;
    set_fill_paint vg ~paint:bg;
    fill vg;

    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+0.5) ~y:(y+0.5) ~w:(w-1.) ~h:(h-1.) ~r:3.5;
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:48);
    stroke vg;
;;

let draw_edit_box (vg : NVG.t) text x y w h =
    let open NVG in
    let open FloatOps in

    draw_edit_box_base vg x y w h;

    Text.set_size vg ~size:17.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:64);
    Text.set_align vg ~align:Align.(left lor middle);
    Text.text vg ~x:(x+h*0.3) ~y:(y+h*0.5) text;
;;

let draw_edit_box_num (vg : NVG.t) text units x y w h =
    let open NVG in
    let open FloatOps in

    draw_edit_box_base vg x y w h;

    let uw = Text.bounds vg ~x:0. ~y:0. units in

    Text.set_size vg ~size:15.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:64);
    Text.set_align vg ~align:Align.(right lor middle);
    Text.text vg ~x:(x+w-h*0.3) ~y:(y+h*0.5) units;

    Text.set_size vg ~size:17.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:128);
    Text.set_align vg ~align:Align.(right lor middle);
    Text.text vg ~x:(x+w-uw.advance-h*0.5) ~y:(y+h*0.5) text;
;;

let draw_button (vg : NVG.t) preicon text x y w h col =
    let open NVG in
    let open FloatOps in

    let corner_radius = 4. in

    let bg = Paint.linear_gradient vg ~sx:x ~sy:y ~ex:x ~ey:(y+h) 
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:(if is_black col then 16 else 32))
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:(if is_black col then 16 else 32))
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+1.) ~y:(y+1.) ~w:(w-2.) ~h:(h-2.) ~r:(corner_radius-1.);
    if not (is_black col) then (
        set_fill_color vg ~color:col;
        fill vg;
    );
    set_fill_paint vg ~paint:bg;
    fill vg;

    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+0.5) ~y:(y+0.5) ~w:(w-1.) ~h:(h-1.) ~r:(corner_radius-0.5);
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:48);
    stroke vg;

    Text.set_size vg ~size:17.;
    Text.set_font_face vg ~name:"sans-bold";
    let tw = (Text.bounds vg ~x:0. ~y:0. text).advance in
    let iw = ref 0. in
    begin match preicon with
    | None -> ()
    | Some icon ->
        Text.set_size vg ~size:(h*1.3); 
        Text.set_font_face vg ~name:"icons";
        iw := (Text.bounds vg ~x:0. ~y:0. icon).advance;
        iw := !iw + h*0.15;

        set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:96);
        Text.set_align vg ~align:Align.(left lor middle);
        Text.text vg ~x:(x+w*0.5-tw*0.5- !iw*0.75) ~y:(y+h*0.5) icon;
    end;

    Text.set_size vg ~size:17.;
    Text.set_font_face vg ~name:"sans-bold";
    Text.set_align vg ~align:Align.(left lor middle);
    set_fill_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:160);
    Text.text vg ~x:(x+w*0.5-tw*0.5+ !iw*0.25) ~y:(y+h*0.5-1.) text;
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:160);
    Text.text vg ~x:(x+w*0.5-tw*0.5+ !iw*0.25) ~y:(y+h*0.5) text;
;;

let icon_check = "\xe2\x9c\x93"

let draw_checkbox (vg : NVG.t) text x y _w h =
    let open NVG in
    let open FloatOps in

    Text.set_size vg ~size:15.;
    Text.set_font_face vg ~name:"sans";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:160);
    Text.set_align vg ~align:Align.(left lor middle);
    Text.text vg ~x:(x+28.) ~y:(y+h*0.5) text;

    let bg = Paint.box_gradient vg ~x:(x+1.) ~y:(y+(h*0.5)-9.+1.) ~w:18. ~h:18.
        ~r:3. ~f:3.
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+1.) ~y:(y+(h*0.5)-9.) ~w:18. ~h:18. ~r:3.;
    set_fill_paint vg ~paint:bg;
    fill vg;

    Text.set_size vg ~size:33.;
    Text.set_font_face vg ~name:"icons";
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:128);
    Text.set_align vg ~align:Align.(center lor middle);
    Text.text vg ~x:(x+9.+2.) ~y:(y+h*0.5) (icon_check);
;;

let draw_slider (vg : NVG.t) pos x y w h =
    let open NVG in
    let open FloatOps in

    let cy = y + (h*0.5) in
    let kr = h * 0.25 in

    save vg;

    let bg = Paint.box_gradient vg ~x ~y:(cy-2.+1.) ~w ~h:4. ~r:2. ~f:2.
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x ~y:(cy-2.) ~w ~h:4. ~r:2.;
    set_fill_paint vg ~paint:bg;
    fill vg;

    let bg = Paint.radial_gradient vg ~cx:(x+pos*w) ~cy:(cy+1.)
        ~in_radius:(kr-3.)
        ~out_radius:(kr+3.)
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:64)
        ~ocol:Color.transparent
    in
    Path.begin_ vg;
    Path.rect vg ~x:(x+pos*w-kr-5.) ~y:(cy-kr-5.) ~w:(kr*2.+5.+5.) ~h:(kr*2.+5.+5.+3.);
    Path.circle vg ~cx:(x+pos*w) ~cy ~r:kr;
    Path.winding vg ~winding:Winding.CW;
    set_fill_paint vg ~paint:bg;
    fill vg;

    let knob = Paint.linear_gradient vg ~sx:x ~sy:(cy-kr) ~ex:x ~ey:(cy+kr) 
        ~icol:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:16)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:16)
    in
    Path.begin_ vg;
    Path.circle vg ~cx:(x+pos*w) ~cy ~r:(kr-1.);
    set_fill_color vg ~color:(Color.rgba ~r:40 ~g:43 ~b:48 ~a:255);
    fill vg;
    set_fill_paint vg ~paint:knob;
    fill vg;

    Path.begin_ vg;
    Path.circle vg ~cx:(x+pos*w) ~cy ~r:(kr-0.5);
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92);
    stroke vg;

    restore vg;
;;

let draw_graph (vg : NVG.t) x y w h t =
    let open NVG in
    let open FloatOps in
    let samples = Array.make 6 0. in

    samples.(0) <- (1. + Float.(sin (t * 1.2345  + (cos (t * 0.33457))*0.44)))*0.5;
    samples.(1) <- (1. + Float.(sin (t * 0.68363 + (cos (t * 1.3))*1.55)))*0.5;
    samples.(2) <- (1. + Float.(sin (t * 1.1642  + (cos (t * 0.33457))*1.24)))*0.5;
    samples.(3) <- (1. + Float.(sin (t * 0.56345 + (cos (t * 1.63))*0.14)))*0.5;
    samples.(4) <- (1. + Float.(sin (t * 1.6245  + (cos (t * 0.254))*0.3)))*0.5;
    samples.(5) <- (1. + Float.(sin (t * 0.345   + (cos (t * 0.03))*0.6)))*0.5;

    let sx = Array.make 6 0. in
    let sy = Array.make 6 0. in

    let dx = w / 5. in

    for i=0 to 5 do
        sx.(i) <- x + (float i)*dx;
        sy.(i) <- y + h*samples.(i)*0.8;
    done;

    (* Graph background *)
    let bg = Paint.linear_gradient vg ~sx:x ~sy:y ~ex:x ~ey:(y+h)
        ~icol:(Color.rgba ~r:0 ~g:160 ~b:192 ~a:0)
        ~ocol:(Color.rgba ~r:0 ~g:160 ~b:192 ~a:64)
    in
    Path.begin_ vg;
    Path.move_to vg ~x:sx.(0) ~y:sy.(0);
    for i=1 to 5 do
        Path.bezier_to vg 
            ~c1x:(sx.(i-.1)+dx*0.5)
            ~c1y:sy.(i-.1)
            ~c2x:(sx.(i)-dx*0.5)
            ~c2y:sy.(i)
            ~x:sx.(i)
            ~y:sy.(i)
    done;
    Path.line_to vg ~x:(x+w) ~y:(y+h);
    Path.line_to vg ~x ~y:(y+h);
    set_fill_paint vg ~paint:bg;
    fill vg; 

    (* Graph line *)
    Path.begin_ vg;
    Path.move_to vg ~x:sx.(0) ~y:(sy.(0)+2.);
    for i=1 to 5 do
        Path.bezier_to vg
            ~c1x:(sx.(i-.1)+dx*0.5)
            ~c1y:(sy.(i-.1)+2.)
            ~c2x:(sx.(i)-dx*0.5)
            ~c2y:(sy.(i)+2.)
            ~x:sx.(i)
            ~y:(sy.(i)+2.)
    done;
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32);
    set_stroke_width vg ~width:3.;
    stroke vg;

    Path.begin_ vg;
    Path.move_to vg ~x:sx.(0) ~y:sy.(0);
    for i=1 to 5 do
        Path.bezier_to vg
            ~c1x:(sx.(i-.1)+dx*0.5)
            ~c1y:sy.(i-.1)
            ~c2x:(sx.(i)-dx*0.5)
            ~c2y:sy.(i)
            ~x:sx.(i)
            ~y:sy.(i)
    done;
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:160 ~b:192 ~a:255);
    set_stroke_width vg ~width:3.;
    stroke vg;

    (* Sample points *)
    for i=0 to 5 do
        let bg = Paint.radial_gradient vg
            ~cx:sx.(i)
            ~cy:sy.(i)
            ~in_radius:3.
            ~out_radius:8.
            ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32)
            ~ocol:Color.transparent
        in
        Path.begin_ vg;
        Path.rect vg ~x:(sx.(i)-10.) ~y:(sy.(i)-10.+2.) ~w:20. ~h:20.;
        set_fill_paint vg ~paint:bg;
        fill vg;
    done;

    Path.begin_ vg;
    for i=0 to 5 do
        Path.circle vg ~cx:sx.(i) ~cy:sy.(i) ~r:4.
    done;
    set_fill_color vg ~color:(Color.rgba ~r:0 ~g:160 ~b:192 ~a:255);
    fill vg;

    Path.begin_ vg;
    for i=0 to 5 do
        Path.circle vg ~cx:sx.(i) ~cy:sy.(i) ~r:2.
    done;
    set_fill_color vg ~color:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:255);
    fill vg;

    set_stroke_width vg ~width:1.
;;

let draw_lines (vg : NVG.t) x y w t =
    let open NVG in
    let open FloatOps in

    let pad = 5. in
    let s = w / 9. - pad*2. in

    let pts = Array.make 8 0. in
    save vg;

    pts.(0) <- ~-.s*0.25 + (Float.cos (t*0.3))*s*0.5;
    pts.(1) <- (Float.sin (t*0.3))*s*0.5;
    pts.(2) <- ~-.s*0.25;
    pts.(3) <- 0.;
    pts.(4) <- s*0.25;
    pts.(5) <- 0.;
    pts.(6) <- s*0.25 + (Float.cos (~-.t*0.3))*s*0.5;
    pts.(7) <- (Float.sin (~-.t*0.3))*s*0.5;

    let joins = NVG.LineJoin.[|Miter; Round; Bevel|] in
    let caps = NVG.LineCap.[|Butt; Round; Square|] in

    for i=0 to 2 do
        for j=0 to 2 do
            let fx = x + s*0.5 + (float (i*.3+.j))/9.*w + pad in
            let fy = y - s*0.5 + pad in

            set_line_cap vg ~cap:caps.(i);
            set_line_join vg ~join:joins.(j);

            set_stroke_width vg ~width:(s*0.3);
            set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:160);
            Path.begin_ vg;
            Path.move_to vg ~x:(fx+pts.(0)) ~y:(fy+pts.(1));
            Path.line_to vg ~x:(fx+pts.(2)) ~y:(fy+pts.(3));
            Path.line_to vg ~x:(fx+pts.(4)) ~y:(fy+pts.(5));
            Path.line_to vg ~x:(fx+pts.(6)) ~y:(fy+pts.(7));
            stroke vg;

            set_line_cap vg ~cap:Butt;
            set_line_join vg ~join:Bevel;

            set_stroke_width vg ~width:1.;
            set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:192 ~b:255 ~a:255);
            Path.begin_ vg;
            Path.move_to vg ~x:(fx+pts.(0)) ~y:(fy+pts.(1));
            Path.line_to vg ~x:(fx+pts.(2)) ~y:(fy+pts.(3));
            Path.line_to vg ~x:(fx+pts.(4)) ~y:(fy+pts.(5));
            Path.line_to vg ~x:(fx+pts.(6)) ~y:(fy+pts.(7));
            stroke vg;
        done;
    done;

    restore vg;
;;

let draw_widths (vg : NVG.t) x y width =
    let open NVG in
    let open FloatOps in

    save vg;

    set_stroke_color vg ~color:Color.black;

    let y = ref y in
    for i=0 to 19 do
        let w = (float i + 0.5)*0.1 in
        set_stroke_width vg ~width:w;
        Path.begin_ vg;
        Path.move_to vg ~x ~y:!y;
        Path.line_to vg ~x:(x+width) ~y:(!y+width*0.3);
        stroke vg;
        y := !y + 10.;
    done;

    restore vg;
;;

let draw_caps (vg : NVG.t) x y width =
    let open NVG in
    let open FloatOps in
    let caps = NVG.LineCap.[|Butt; Round; Square|] in
    let line_width = 8. in

    save vg;

    Path.begin_ vg;
    Path.rect vg ~x:(x-line_width/2.) ~y ~w:(width+line_width) ~h:40.;
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:32);
    fill vg;

    Path.begin_ vg;
    Path.rect vg ~x ~y ~w:width ~h:40.;
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:32);
    fill vg;

    set_stroke_width vg ~width:line_width;
    for i=0 to 2 do
        set_line_cap vg ~cap:caps.(i);
        set_stroke_color vg ~color:Color.black;
        Path.begin_ vg;
        Path.move_to vg ~x ~y:(y + float i * 10. + 5.);
        Path.line_to vg ~x:(x+width) ~y:(y + float i * 10. + 5.);
        stroke vg;
    done;

    restore vg;
;;

let draw_color_wheel (vg : NVG.t) x y w h t =
    let open NVG in
    let open FloatOps in
    let hue = Float.sin (t * 0.12) in

    save vg;

    let cx = x + w*0.5 in
    let cy = y + h*0.5 in
    let r1 = (if w < h then w else h) * 0.5 - 5.0 in
    let r0 = r1 - 20. in
    let aeps = 0.5 / r1 in (* half pixel arc-length in radians 2pi cancels out *)
    
    for i=0 to 5 do
        let a0 = float i / 6. * Float.pi * 2. - aeps in
        let a1 = float (i+.1) / 6. * Float.pi * 2. + aeps in
        Path.begin_ vg;
        Path.arc vg ~cx ~cy ~r:r0 ~a0 ~a1 ~dir:Winding.CW;
        Path.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 ~dir:Winding.CCW;
        Path.close vg;

        let ax = cx + Float.(cos a0) * (r0 + r1)*0.5 in
        let ay = cy + Float.(sin a0) * (r0 + r1)*0.5 in
        let bx = cx + Float.(cos a1) * (r0 + r1)*0.5 in
        let by = cy + Float.(sin a1) * (r0 + r1)*0.5 in
        let paint = Paint.linear_gradient vg ~sx:ax ~sy:ay ~ex:bx ~ey:by 
            ~icol:(Color.hsla ~h:(a0/(Float.pi*2.)) ~s:1. ~l:0.55 ~a:255)
            ~ocol:(Color.hsla ~h:(a1/(Float.pi*2.)) ~s:1. ~l:0.55 ~a:255)
        in
        set_fill_paint vg ~paint;
        fill vg;
    done;

    Path.begin_ vg;
    Path.circle vg ~cx ~cy ~r:(r0-0.5);
    Path.circle vg ~cx ~cy ~r:(r1+0.5);
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:64);
    set_stroke_width vg ~width:1.;
    stroke vg;

    (* Selector *)
    save vg;
    Transform.translate vg ~x:cx ~y:cy;
    Transform.rotate vg ~angle:(hue*Float.pi*2.);

    (* Marker on *)
    set_stroke_width vg ~width:2.;
    Path.begin_ vg;
    Path.rect vg ~x:(r0-1.) ~y:~-.3. ~w:(r1-r0+2.) ~h:6.;
    set_stroke_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:192);
    stroke vg;

    let paint = Paint.box_gradient vg ~x:(r0-3.) ~y:~-.5. ~w:(r1-r0+6.) ~h:10.
        ~r:2.
        ~f:4.
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128)
        ~ocol:Color.transparent
    in
    Path.begin_ vg;
    Path.rect vg ~x:(r0-2.-10.) ~y:(-4.-10.) ~w:(r1-r0+4.+20.) ~h:(8.+20.);
    Path.rect vg ~x:(r0-2.) ~y:~-.4. ~w:(r1-r0+4.) ~h:8.;
    Path.winding vg ~winding:Winding.CW;
    set_fill_paint vg ~paint;
    fill vg;

    (* Center triangle *)
    let r = r0 - 6. in
    let ax = Float.(cos (120. / 180. * Float.pi))*r in
    let ay = Float.(sin (120. / 180. * Float.pi))*r in
    let bx = Float.(cos (-120. / 180.0 * Float.pi))*r in
    let by = Float.(sin (-120. / 180.0 * Float.pi))*r in
    Path.begin_ vg;
    Path.move_to vg ~x:r ~y:0.;
    Path.line_to vg ~x:ax ~y:ay;
    Path.line_to vg ~x:bx ~y:by;
    Path.close vg;

    let paint = Paint.linear_gradient vg ~sx:r ~sy:0. ~ex:ax ~ey:ay
        ~icol:(Color.hsla ~h:hue ~s:1. ~l:0.5 ~a:255)
        ~ocol:Color.white
    in
    set_fill_paint vg ~paint;
    fill vg;

    let paint = Paint.linear_gradient vg ~sx:((r+ax)*0.5) ~sy:(ay*0.5) ~ex:bx ~ey:by
        ~icol:Color.transparent
        ~ocol:Color.black
    in
    set_fill_paint vg ~paint;
    fill vg;
    set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:64);
    stroke vg;

    (* Select circle on triangle *)
    let ax = (Float.cos (120. / 180. * Float.pi))*r*0.3 in
    let ay = (Float.sin (120. / 180. * Float.pi))*r*0.4 in
    set_stroke_width vg ~width:2.;
    Path.begin_ vg;
    Path.circle vg ~cx:ax ~cy:ay ~r:5.;
    set_stroke_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:192);
    stroke vg;

    let paint = Paint.radial_gradient vg ~cx:ax ~cy:ay 
        ~in_radius:7.
        ~out_radius:9.
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:64)
        ~ocol:Color.transparent
    in
    Path.begin_ vg;
    Path.rect vg ~x:(ax-20.) ~y:(ay-20.) ~w:40. ~h:40.;
    Path.circle vg ~cx:ax ~cy:ay ~r:7.;
    Path.winding vg ~winding:Winding.CW;
    set_fill_paint vg ~paint;
    fill vg;

    restore vg;
    restore vg;
;;

let draw_scissor (vg : NVG.t) x y t =
    let open NVG in

    save vg;

    Transform.translate vg ~x ~y;
    Transform.rotate vg ~angle:(Transform.deg_to_rad 5.);
    Path.begin_ vg;
    Path.rect vg ~x:(~-.20.) ~y:(~-.20.) ~w:60. ~h:40.;
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:0 ~b:0 ~a:255);
    fill vg;
    Scissor.scissor vg ~x:(~-.20.) ~y:(~-.20.) ~w:60. ~h:40.;

    Transform.translate vg ~x:40. ~y:0.;
    Transform.rotate vg ~angle:t;

    save vg;
    Scissor.reset vg;
    Path.begin_ vg;
    Path.rect vg ~x:(~-.20.) ~y:(~-.10.) ~w:60. ~h:30.;
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:128 ~b:0 ~a:64);
    fill vg;
    restore vg;

    Scissor.intersect vg ~x:(~-.20.) ~y:(~-.10.) ~w:60. ~h:30.;
    Path.begin_ vg;
    Path.rect vg ~x:(~-.20.) ~y:(~-.10.) ~w:60. ~h:30.;
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:128 ~b:0 ~a:255);
    fill vg;

    restore vg;
;;

let draw_spinner (vg : NVG.t) cx cy r t =
    let open NVG in
    let open FloatOps in
    
    let a0 = 0.0 + t*6. in
    let a1 = Float.pi + t*6. in
    let r0 = r in
    let r1 = r * 0.75 in

    save vg;

    Path.begin_ vg;
    Path.arc vg ~cx ~cy ~r:r0 ~a0 ~a1 ~dir:Winding.CW;
    Path.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 ~dir:Winding.CCW;
    Path.close vg;

    let ax = cx + Float.cos a0 * (r0+r1)*0.5 in
    let ay = cy + Float.sin a0 * (r0+r1)*0.5 in
    let bx = cx + Float.cos a1 * (r0+r1)*0.5 in
    let by = cy + Float.sin a1 * (r0+r1)*0.5 in
    let paint = Paint.linear_gradient vg ~sx:ax ~sy:ay ~ex:bx ~ey:by 
        ~icol:Color.transparent
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128)
    in
    set_fill_paint vg ~paint;
    fill vg;

    restore vg;
;;

type data = {
    images : NVG.Image.image array;
}

let draw_thumbnails (vg : NVG.t) x y w h images t =
    let open NVG in
    let open FloatOps in

    let corner_radius = 3. in
    let thumb = 60. in
    let arry = 30.5 in
    let len = Array.length images |> float in
    let stack_h = (len/2.) * (thumb+10.) + 10. in
    let u = (1. + (Float.cos (t*0.5)))*0.5 in
    let u2 = (1. + (Float.cos (t*0.2)))*0.5 in

    save vg;

    let shadow_paint = Paint.box_gradient vg ~x ~y:(y+4.) ~w ~h
        ~r:(corner_radius*2.) 
        ~f:20.
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128)
        ~ocol:Color.transparent
    in
    Path.begin_ vg;
    Path.rect vg ~x:(x-10.) ~y:(y-10.) ~w:(w+20.) ~h:(h+30.);
    Path.rounded_rect vg ~x ~y ~w ~h ~r:corner_radius;
    Path.winding vg ~winding:Winding.CW;
    set_fill_paint vg ~paint:shadow_paint;
    fill vg;

    Path.begin_ vg;
    Path.rounded_rect vg ~x ~y ~w ~h ~r:corner_radius;
    Path.move_to vg ~x:(x-10.) ~y:(y+arry);
    Path.line_to vg ~x:(x+1.) ~y:(y+arry-11.);
    Path.line_to vg ~x:(x+1.) ~y:(y+arry+11.);
    set_fill_color vg ~color:(Color.rgba ~r:200 ~g:200 ~b:200 ~a:255);
    fill vg;

    save vg;
    Scissor.scissor vg ~x ~y ~w ~h;
    Transform.translate vg ~x:0. ~y:(~-.(stack_h - h)*u);

    let dv = 1. / (len - 1.) in

    for i=0 to (Array.length images) -. 1 do
        let tx = x + 10. in
        let ty = y + 10. in
        let tx = tx + float (i mod 2) * (thumb + 10.) in
        let ty = ty + float (i /. 2) * (thumb + 10.) in
        let imgw, imgh = Image.size vg images.(i) in
        let iw, ih, ix, iy =
            if imgw <. imgh then (
                let iw = thumb in
                let ih = iw * float imgh / float imgw in 
                let ix = 0. in
                let iy = ~-.(ih - thumb) * 0.5 in
                iw, ih, ix, iy
            ) else (
                let ih = thumb in
                let iw = ih * float imgw / float imgh in
                let ix = ~-.(iw - thumb) * 0.5 in
                let iy = 0. in
                iw, ih, ix, iy
            )
        in

        let v = float i * dv in
        let a = clampf ((u2-v)/dv) 0. 1. in

        if a < 1. then (
            draw_spinner vg (tx + thumb/2.) (ty + thumb/2.) (thumb*0.25) t;
        );

        let img_paint = Paint.image_pattern vg 
            ~cx:(tx+ix) ~cy:(ty+iy) 
            ~w:iw ~h:ih
            ~angle:0.
            ~image:images.(i)
            ~alpha:a
        in
        Path.begin_ vg;
        Path.rounded_rect vg ~x:tx ~y:ty ~w:thumb ~h:thumb ~r:5.;
        set_fill_paint vg ~paint:img_paint;
        fill vg;

        let shadow_paint = Paint.box_gradient vg ~x:(tx-1.) ~y:ty ~w:(thumb+2.) ~h:(thumb+2.)
            ~r:5. ~f:3.
            ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128)
            ~ocol:Color.transparent
        in
        Path.begin_ vg;
        Path.rect vg ~x:(tx-5.) ~y:(ty-5.) ~w:(thumb+10.) ~h:(thumb+10.);
        Path.rounded_rect vg ~x:tx ~y:ty ~w:thumb ~h:thumb ~r:6.;
        Path.winding vg ~winding:Winding.CW;
        set_fill_paint vg ~paint:shadow_paint;
        fill vg;

        Path.begin_ vg; 
        Path.rounded_rect vg ~x:(tx+0.5) ~y:(ty+0.5) ~w:(thumb-1.) ~h:(thumb-1.) ~r:3.5;
        set_stroke_width vg ~width:1.;
        set_stroke_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:192);
        stroke vg;
    done;

    restore vg;

    (* Hide fades *)

    let fade_paint = Paint.linear_gradient vg ~sx:x ~sy:y ~ex:x ~ey:(y+6.)
        ~icol:(Color.rgba ~r:200 ~g:200 ~b:200 ~a:255)
        ~ocol:(Color.rgba ~r:200 ~g:200 ~b:200 ~a:0)
    in
    Path.begin_ vg;
    Path.rect vg ~x:(x+4.) ~y ~w:(w-8.) ~h:6.;
    set_fill_paint vg ~paint:fade_paint;
    fill vg;

    let fade_paint = Paint.linear_gradient vg ~sx:x ~sy:(y+h) ~ex:x ~ey:(y+h-6.)
        ~icol:(Color.rgba ~r:200 ~g:200 ~b:200 ~a:255)
        ~ocol:(Color.rgba ~r:200 ~g:200 ~b:200 ~a:0)
    in
    Path.begin_ vg;
    Path.rect vg ~x:(x+4.) ~y:(y+h-6.) ~w:(w-8.) ~h:6.;
    set_fill_paint vg ~paint:fade_paint;
    fill vg;

    (* Scroll bar *)
    let shadow_paint = Paint.box_gradient vg ~x:(x+w-12.+1.) ~y:(y+4.+1.) ~w:8. ~h:(h-8.)
        ~r:3. ~f:4.
        ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32)
        ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+w-12.) ~y:(y+4.) ~w:8. ~h:(h-8.) ~r:3.;
    set_fill_paint vg ~paint:shadow_paint;
    fill vg;

    let scroll_h = (h / stack_h) * (h - 8.) in
    let shadow_paint = Paint.box_gradient vg ~x:(x+w-12.-1.) ~y:(y+4.+(h-8.-scroll_h)*u-1.)
        ~w:8. ~h:scroll_h
        ~r:3. ~f:4.
        ~icol:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:255)
        ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:255)
    in
    Path.begin_ vg;
    Path.rounded_rect vg ~x:(x+w-12.+1.) ~y:(y+4.+1.+(h-8.-scroll_h)*u) ~w:(8.-2.) ~h:(scroll_h-2.) ~r:2.;
    set_fill_paint vg ~paint:shadow_paint;
    fill vg;

    restore vg;
;;

let text = "This is longer chunk of text.\n  \n  Would have used lorem ipsum but she    was busy jumping over the lazy dog with the fox and all the men who came to the aid of the party.🎉" 

let hover_text = "Hover your mouse over the text to see calculated caret position" 
let lines = NVG.Text.make_empty_rows 3
let glyphs = Array.make (String.length text) NVG.Text.empty_glyph_position

let draw_paragraph (vg : NVG.t) x y width _height mx my =
    let open NVG in
    let open FloatOps in


    save vg;

    Text.set_size vg ~size:15.;
    Text.set_font_face vg ~name:"sans";
    Text.set_align vg ~align:Align.(left lor top);
    let line_h = (Text.metrics vg).line_height in

    let start = ref 0 in
    let end_ = String.length text in
    let y = ref y in
    let lnum = ref 0 in
    let gutter = ref 0 in

    let gx = ref 0. in
    let gy = ref 0. in

    let r = ref 0 in

    let rec loop() =
        let rows = Text.break_lines vg ~break_width:width ~max_rows:3 ~start:!start ~end_:end_ ~lines text in
        match rows with
        | 0 -> ()
        | count ->
            for i=0 to count-.1 do
                let row = lines.(i) in
                let hit = mx > x && mx < (x + width) && my >= !y && my < (!y + line_h) in
                incr r;

                Path.begin_ vg;
                set_fill_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:(if hit then 64 else 16));
                Path.rect vg ~x:(x+row.minx) ~y:!y ~w:(row.maxx - row.minx) ~h:line_h;
                fill vg;

                set_fill_color vg ~color:Color.white;
                Text.text vg ~x ~y:!y ~start:row.start_index ~end_:row.end_index text;

                if hit then (
                    let caretx = ref (if mx < (x+row.width/2.) then x else x+row.width) in
                    let px = ref x in
                    let nglyphs = Text.glyph_positions vg ~x ~y:!y 
                        ~start:row.start_index  
                        ~end_:row.end_index 
                        ~glyphs
                        text 
                    in
                    for i=0 to nglyphs-.1 do
                        let glyph = glyphs.(i) in
                        let x0 = glyph.x in
                        let x1 = if i+.1 <. nglyphs then (glyphs.(i+.1)).x else (x+row.width) in
                        let gx = x0 * 0.3 + x1 * 0.7 in
                        if mx >= !px && mx < gx then (
                            caretx := glyph.x;
                        );
                        px := gx;
                    done;
                    Path.begin_ vg;
                    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:192 ~b:0 ~a:255);
                    Path.rect vg ~x:!caretx ~y:!y ~w:2. ~h:line_h;
                    fill vg;

                    gutter := !lnum+.1;
                    gx := x - 10.;
                    gy := !y + line_h/2.;
                );

                incr lnum;
                y := !y + line_h;
                start := row.next;
            done;
            loop();
    in
    loop();

    if !gutter >. 0 then (
        let txt = Printf.sprintf "%d" !gutter in
        Text.set_size vg ~size:12.;
        Text.set_align vg ~align:Align.(right lor middle);
        let b = Text.bounds vg ~x:!gx ~y:!gy txt in
        Path.begin_ vg;
        set_fill_color vg ~color:(Color.rgba ~r:255 ~g:192 ~b:0 ~a:255);
        Path.rounded_rect vg ~x:(b.box.xmin-4.) ~y:(b.box.ymin-2.)
            ~w:((b.box.xmax - b.box.xmin)+8.)
            ~h:((b.box.ymax - b.box.ymin)+4.)
            ~r:(((b.box.ymax - b.box.ymin)+4.)/2.-1.)
            ;
        fill vg;

        set_fill_color vg ~color:(Color.rgba ~r:32 ~g:32 ~b:32 ~a:255);
        Text.text vg ~x:!gx ~y:!gy txt
    );

    y := !y + 20.;

    Text.set_size vg ~size:11.;
    Text.set_align vg ~align:Align.(left lor top);
    Text.set_line_height vg ~height:1.2;

    let bounds = Text.box_bounds vg ~x ~y:!y ~break_width:150. hover_text in

    let gx = (clampf mx bounds.xmin bounds.xmax) - mx in
    let gy = (clampf my bounds.ymin bounds.ymax) - my in
    let a = Float.sqrt (gx*gx + gy*gy) / 30. in
    let a = clampf a 0. 1. in
    Global.set_alpha vg ~alpha:a;

    Path.begin_ vg;
    set_fill_color vg ~color:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:255);
    Path.rounded_rect vg ~x:(bounds.xmin-2.) ~y:(bounds.ymin-2.)
        ~w:((bounds.xmax-bounds.xmin)+4.)
        ~h:((bounds.ymax-bounds.ymin)+4.)
        ~r:3.;
    let px = (bounds.xmax + bounds.xmin) / 2. in
    Path.move_to vg ~x:px ~y:(bounds.ymin-10.);
    Path.line_to vg ~x:(px+7.) ~y:(bounds.ymin+1.);
    Path.line_to vg ~x:(px-7.) ~y:(bounds.ymin+1.);
    fill vg;

    set_fill_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:220);
    Text.text_box vg ~x ~y:!y ~break_width:150. hover_text;

    restore vg;
;;

let icon_login = 0xE740
let icon_trash = 0xE729

let render_demo (vg : NVG.t) mx my width height t blowup data =
    let open NVG in
    let open FloatOps in
    draw_eyes vg (width - 250.) 50. 150. 100. mx my t;
    draw_paragraph vg (width - 450.) 50. 150. 100. mx my;
    draw_graph vg 0. (height/2.) width (height/2.) t;
    draw_color_wheel vg (width - 300.) (height - 300.) 250. 250. t;

    (* Line joins *)
    draw_lines vg 120. (height - 50.) 600. t;

    draw_widths vg 10. 50. 30.;

    (* Line caps *)
    draw_caps vg 10. 300. 30.;

    draw_scissor vg 50. (height - 80.) t;

    NVG.save vg;

    if blowup then (
        NVG.Transform.rotate vg ~angle:(Float.sin (t*0.3) * 5. / 180. * Float.pi);
        NVG.Transform.scale vg ~x:2. ~y:2.;
    );

    draw_window vg "Widgets 'n stuff" 50. 50. 300. 400.;

    let x = ref 60.
    and y = ref 95. in

    draw_search_box vg "Search" !x !y 280. 25.;
    y := !y + 40.;
    draw_drop_down vg "Effects" !x !y 280. 28.;
    let popy = !y + 14. in
    y := !y + 45.;

    draw_label vg "Login" !x !y 280. 20.;
    y := !y + 25.;
    draw_edit_box vg "Email" !x !y 280.0 28.;
    y := !y + 35.;
    draw_edit_box vg "Password" !x !y 280.0 28.;
    y := !y + 38.;

    draw_checkbox vg "Remember me" !x !y 140. 28.;
    draw_button vg (Some (to_utf8 icon_login)) "Sign In" (!x+138.) !y 140. 28. (Color.rgba ~r:0 ~g:96 ~b:128 ~a:255);
    y := !y + 45.;

    draw_label vg "Diameter" !x !y 280. 20.;
    y := !y + 25.;
    draw_edit_box_num vg "123.00" "px" (!x+180.) !y 100. 28.;
    draw_slider vg 0.4 !x !y 170. 28.;
    y := !y + 55.;

    draw_button vg (Some (to_utf8 icon_trash)) "Delete" !x !y 160. 28. (Color.rgba ~r:128 ~g:16 ~b:8 ~a:255);
    draw_button vg None "Cancel" (!x+170.) !y 110. 28. (Color.transparent);

    (* Thumbnails box *)
    draw_thumbnails vg 365. (popy - 30.) 160. 300. data.images t;

    NVG.restore vg;
;;

