type style = FPS
           | Ms
           | Percent

type t = {
    style : style;
    name : string;
    values : float array; 
    mutable head : int;
}

let init style name = {
    name;    
    style;
    values = Array.make 100 0.;
    head = 0;
}

let average t =
    let avg = ref 0. in
    for i=0 to Array.length t.values-1 do
        avg := !avg +. t.values.(i)
    done;
    !avg /. float (Array.length t.values)
;;

let update t dt =
    t.head <- (t.head + 1) mod (Array.length t.values);
    t.values.(t.head) <- dt;
;;

let render t (vg : NVG.t) x y =
    let avg = average t in
    let w = 200. in
    let h = 35. in

    let open NVG in
    let open FloatOps in
    Path.begin_ vg;
    Path.rect vg ~x ~y ~w ~h;
    set_fill_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128);
    fill vg;

    Path.begin_ vg;
    Path.move_to vg ~x ~y:(y+h);
    let len = Array.length t.values in
    begin match t.style with
    | FPS ->
        for i=0 to len-.1 do
            let v = 1. / (0.00001 + t.values.((t.head+.i) mod len)) in
            let v = if v > 80. then 80. else v in
            let vx = x + (float i / (float len - 1.)) * w in
            let vy = y + h - ((v / 80.) * h) in
            Path.line_to vg ~x:vx ~y:vy
        done;
    | Percent -> ()
    | Ms -> ()
    end;
    Path.line_to vg ~x:(x+w) ~y:(y+h);
    set_fill_color vg ~color:(Color.rgba ~r:255 ~g:192 ~b:0 ~a:128);
    fill vg;

    Text.set_font_face vg ~name:"mono";

    Text.set_size vg ~size:12.;
    Text.set_align vg ~align:Align.(left lor top);
    set_fill_color vg ~color:(Color.rgba ~r:240 ~g:240 ~b:240 ~a:192);
    Text.text vg ~x:(x+3.) ~y:(y+3.) t.name;

    begin match t.style with
    | FPS -> 
        Text.set_size vg ~size:15.;
        Text.set_align vg ~align:Align.(right lor top);
        set_fill_color vg ~color:(Color.rgba ~r:240 ~g:240 ~b:240 ~a:255);
        let s = Printf.sprintf "%.2f FPS" (1. / avg) in
        Text.text vg ~x:(x+w-3.) ~y:(y+3.) s;

        Text.set_size vg ~size:13.;
        Text.set_align vg ~align:Align.(right lor baseline);
        set_fill_color vg ~color:(Color.rgba ~r:240 ~g:240 ~b:240 ~a:160);
        let s = Printf.sprintf "%.2f ms" (avg * 1000.) in
        Text.text vg ~x:(x+w-3.) ~y:(y+h-3.) s;
    | Percent -> ()
    | Ms -> ()
    end;
;;
