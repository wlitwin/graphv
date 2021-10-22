open Graphv_core_lib.FloatOps

type t = {
    arr : Dyn.t;
    mutable size : int;
}

let create () =
    let arr = Dyn.create 10000 in
    {
        arr;
        size = 0;
    }
;;

let clear t =
    Dyn.clear t.arr;
    t.size <- 0;
;;

let [@inline always] iteri t ~f =
    for i=0 to t.size-.1 do
        f i Dyn.(get t.arr i)
    done
;;

let [@inline always] iter t ~f =
    for i=0 to t.size-.1 do
        f Dyn.(get t.arr i)
    done
;;

let num_verts t =
    t.size
;;

let capacity t =
    Dyn.capacity t.arr /. 4
;;

let [@inline always] iterv t ~f =
    let len = num_verts t -. 1 in
    let rec loop i =
        if (*i >= len*) len <. i then ()
        else (
            let x = Dyn.get t.arr (i*.4+.0) in
            let y = Dyn.get t.arr (i*.4+.1) in
            let u = Dyn.get t.arr (i*.4+.2) in
            let v = Dyn.get t.arr (i*.4+.3) in
            f x y u v;
            loop (i+.1)
        )
    in
    loop 0
;;

let [@inline always] check_size t idx =
    let len = (Dyn.length t.arr lsr 2) -. 1 in
    if (*idx >= len*) len <. idx then (
        let new_len = imax (idx -. len +. 2) 1 in
        Dyn.add_range t.arr (new_len lsl 2) |> ignore
    );
;;


let set t idx x y u v =
    t.size <- imax (idx+.1) t.size;
    check_size t idx;
    let off = idx*.4 in
    Dyn.set t.arr off x;
    Dyn.set t.arr (off+.1) y;
    Dyn.set t.arr (off+.2) u;
    Dyn.set t.arr (off+.3) v;
;;

let get t idx =
    let x = Dyn.get t.arr (idx*.4+.0) in
    let y = Dyn.get t.arr (idx*.4+.1) in
    let u = Dyn.get t.arr (idx*.4+.2) in
    let v = Dyn.get t.arr (idx*.4+.3) in
    x, y, u, v
;;

let num_bytes t =
    t.size *. 16
;;

let num_floats t =
    t.size *. 4
;;

let unsafe_array t =
    Dyn.unsafe_array t.arr

module Sub = struct
    type parent = t
    type t = Dyn.Sub.sub

    let sub p start end_ =
        Dyn.Sub.sub p.arr (start*.4) (end_*.4)
    ;;

    let vertex_offset (t : t) =
        Dyn.Sub.offset t /. 4

    let length t = Dyn.Sub.length t /. 4

    let blit ~src ~dst ~src_start ~dst_start ~len =
        (* Check if the dst is large enough... *)
        check_size dst (dst_start +. len);
        dst.size <- imax (dst.size+.len) dst.size;
        Dyn.Sub.blit
            ~src
            ~dst:dst.arr
            ~src_start:(src_start*.4)
            ~dst_start:(dst_start*.4)
            ~len:(len*.4)
    ;;

    let num_verts t =
        Dyn.Sub.length t /. 4

    let create () =
        Dyn.Sub.sub Dyn.(create 1) 0 1

    let empty = create()
end
