type underlying = Buffer.Float.t

type t = {
    mutable arr : Buffer.Float.t;
    mutable size : int;
}

let create size = {
    arr = Buffer.Float.create size;
    size = 0;
}

let [@inline always] unsafe_array t = t.arr
let [@inline always] length t = t.size
let [@inline always] capacity t = Buffer.Float.length t.arr
let [@inline always] set t i v = Buffer.Float.set t.arr i v
let [@inline always] get t i = Buffer.Float.get t.arr i
let [@inline always] clear t =
    t.size <- 0

let add_range t amount =
    let len = capacity t in
    if t.size + amount >= len then (
        let new_len = (max (t.size+amount) len)*3/2 in
        let new_arr = Buffer.Float.create new_len in
        Buffer.Float.blit ~src:t.arr ~s_off:0 ~dst:new_arr ~d_off:0 ~len:len;
        t.arr <- new_arr
    );
    let offset = t.size in
    t.size <- t.size + amount;
    offset
;;

module Sub = struct
    type sub = {
        off : int;
        len : int;
        t : t;
    }

    let sub (t : t) off len = {
        off; len; t
    }

    let length t = t.len

    let offset t = t.off

    (*
    let get t idx =
        assert (t.off + idx < t.t.size);
        get t.t (t.off + idx)
    ;;
    *)

    let blit ~src ~dst ~(src_start : int) ~(dst_start : int) ~len =
        Buffer.Float.blit ~src:src.t.arr ~s_off:(src_start+src.off) ~dst:dst.arr ~d_off:dst_start ~len
    end

