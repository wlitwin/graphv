open FloatOps

type t = {
    mutable arr : Buffer.Float.t;
    mutable size : int;
}

let create () = {
    arr = Buffer.Float.create 10000;
    size = 0;
}

let clear t =
    t.size <- 0;
;;

let [@inline always] iteri t ~f =
    for i=0 to t.size-.1 do
        f i Buffer.Float.(get t.arr i)
    done
;;

let [@inline always] iter t ~f =
    for i=0 to t.size-.1 do
        f Buffer.Float.(get t.arr i)
    done
;;

let num_verts t =
    t.size
;;

let capacity t =
    Buffer.Float.length t.arr /. 4
;;

let [@inline always] iterv t ~f =
    let len = num_verts t -. 1 in
    let rec loop i =
        if (*i >= len*) len <. i then ()
        else (
            let x = Buffer.Float.get t.arr (i*.4+.0) in
            let y = Buffer.Float.get t.arr (i*.4+.1) in
            let u = Buffer.Float.get t.arr (i*.4+.2) in
            let v = Buffer.Float.get t.arr (i*.4+.3) in
            f x y u v;
            loop (i+.1)
        )
    in
    loop 0
;;

let grow_vertex t idx =
    let len = Buffer.Float.length t.arr in
    let new_len = (len +. (((idx+.1) lsl 4) -. len))*.3/.2 in
    let new_arr = Buffer.Float.create new_len in
    Buffer.Float.blit ~src:t.arr ~s_off:0 ~dst:new_arr ~d_off:0 ~len:len;
    t.arr <- new_arr
;;

let check_size t idx =
    if Buffer.Float.length t.arr <. ((idx+.1) lsl 4) then (
        grow_vertex t idx
    );
    t.size <- imax (idx+.1) t.size;
;;

let set t idx x y u v =
    if Buffer.Float.length t.arr <. ((idx+.1) lsl 4) then (
        grow_vertex t idx;
    );
    t.size <- imax t.size (idx+.1);
    let off = idx*.4 in
    Buffer.Float.set t.arr off x;
    Buffer.Float.set t.arr (off+.1) y;
    Buffer.Float.set t.arr (off+.2) u;
    Buffer.Float.set t.arr (off+.3) v;
;;

let [@inline always] unsafe_set t idx x y u v =
    let off = idx*.4 in
    Buffer.Float.set t.arr off x;
    Buffer.Float.set t.arr (off+.1) y;
    Buffer.Float.set t.arr (off+.2) u;
    Buffer.Float.set t.arr (off+.3) v;
;;

let get t idx =
    let x = Buffer.Float.get t.arr (idx*.4+.0) in
    let y = Buffer.Float.get t.arr (idx*.4+.1) in
    let u = Buffer.Float.get t.arr (idx*.4+.2) in
    let v = Buffer.Float.get t.arr (idx*.4+.3) in
    x, y, u, v
;;

let num_bytes t =
    t.size *. 16
;;

let num_floats t =
    t.size *. 4
;;

let unsafe_array t = t.arr

module Sub = struct
    type parent = t
    type nonrec t = {
        off : int;
        len : int;
        t : t;
    }

    let sub (t : parent) off len = {
        off; len; t
    }

    let length t = t.len
    let vertex_offset t = t.off

    let blit ~src ~dst ~src_start ~dst_start ~len =
        (* Check if the dst is large enough... *)
        check_size dst (dst_start +. len);
        Buffer.Float.blit 
            ~src:src.t.arr 
            ~s_off:((src_start+.src.off)*.4)
            ~dst:dst.arr 
            ~d_off:(dst_start*.4)
            ~len:(len*.4)
    ;;

    let num_verts t = t.len

    let create () =
        sub {
            arr = Buffer.Float.create 1;
            size = 0
        } 0 1
    ;;

    let empty = create()
end
