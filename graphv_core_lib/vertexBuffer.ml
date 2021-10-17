module type DynArrayS = sig
    type t
    type underlying
    val create : int -> t
    val clear : t -> unit
    val get : t -> int -> float
    val set : t -> int -> float -> unit
    val capacity : t -> int
    val length : t -> int
    val add_range : t -> int -> int
    val unsafe_array : t -> underlying

    module Sub : sig
        type sub

        val sub : t -> int -> int -> sub
        val offset : sub -> int
        val length : sub -> int
        val blit : src:sub -> dst:t -> src_start:int -> dst_start:int -> len:int -> unit
    end
end

let max (a : int) (b : int) =
    if a > b then a else b

    (*
module Make(DynArray : DynArrayS) = struct
    type buffer = DynArray.t
    type underlying = DynArray.underlying
type t = {
    arr : buffer;
    mutable size : int;
}

let create () =
    let arr = DynArray.create 1000 in
    {
        arr;
        size = 0;
    }
;;

let clear t = 
    DynArray.clear t.arr;
    t.size <- 0;
;;

let iteri t ~f = 
    for i=0 to t.size-1 do
        f i DynArray.(get t.arr i)
    done
;;

let iter t ~f =
    for i=0 to t.size-1 do
        f DynArray.(get t.arr i)
    done
;;

let num_verts t =
    t.size
;;

let capacity t =
    DynArray.capacity t.arr / 4
;;

let iterv t ~f =
    let len = num_verts t in
    let rec loop i =
        if i >= len then ()
        else (
            let x = DynArray.get t.arr (i*4+0) in
            let y = DynArray.get t.arr (i*4+1) in
            let u = DynArray.get t.arr (i*4+2) in
            let v = DynArray.get t.arr (i*4+3) in
            f x y u v;
            loop (i+1)
        )
    in
    loop 0
;;

let check_size t idx =
    let len = DynArray.length t.arr / 4 in
    if idx >= len then (
        (*if idx > 10000 then (
            failwith "WTF";
        );*)
        (*let new_len = (idx-len)*2 + (((idx-len)*2) mod 4) in
        let new_len = max new_len 88 in
        let new_len = new_len * 4 in*)
        let new_len = max (idx - len + 1) 1 in
        DynArray.add_range t.arr (new_len * 4) |> ignore
    );
;;

(*
let _add t x y u v =
    let idx = DynArray.length t.arr in
    check_size t idx;
    DynArray.set t (idx+0) x;
    DynArray.set t (idx+1) y;
    DynArray.set t (idx+2) u;
    DynArray.set t (idx+3) v;
;;
*)

let set t idx x y u v =
    (*Printf.printf "set %d\n%!" idx;*)
    (*if idx = 8 then ( failwith "ok"; );*)
    t.size <- max (idx+1) t.size;
    check_size t idx;
    let off = idx*4 in
    DynArray.set t.arr off x;
    DynArray.set t.arr (off+1) y;
    DynArray.set t.arr (off+2) u;
    DynArray.set t.arr (off+3) v;
;;

let get t idx =
    let x = DynArray.get t.arr (idx*4+0) in
    let y = DynArray.get t.arr (idx*4+1) in
    let u = DynArray.get t.arr (idx*4+2) in
    let v = DynArray.get t.arr (idx*4+3) in
    x, y, u, v
;;

let num_bytes t =
    t.size * 4 * 4
;;

let num_floats t =
    t.size * 4
;;

let unsafe_array t =
    DynArray.unsafe_array t.arr

(*
let upload (t : t) =
    Printf.printf "Size %d\n%!" t.size;
    Gl.buffer_data 
        Gl.array_buffer 
        (num_bytes t) 
        (Some (DynArray.unsafe_array t.arr)) 
        Gl.stream_draw;
;;
*)

module Sub = struct
    type parent = t
    type t = DynArray.Sub.sub
        (*(float, Bigarray.float32_elt) DynArray.Sub.sub*)

    let sub p start end_: t =
        DynArray.Sub.sub p.arr (start*4) (end_*4)
    ;;

    let vertex_offset (t : t) =
        DynArray.Sub.offset t / 4

    let length t = DynArray.Sub.length t / 4

    let blit ~src ~dst ~src_start ~dst_start ~len =
        (* Check if the dst is large enough... *)
        check_size dst (dst_start + len);
        dst.size <- max (dst.size+len) dst.size;
        DynArray.Sub.blit 
            ~src
            ~dst:dst.arr
            ~src_start:(src_start*4)
            ~dst_start:(dst_start*4)
            ~len:(len*4)
    ;;

    let num_verts t =
        DynArray.Sub.length t / 4

    let create () =
        DynArray.Sub.sub DynArray.(create 1) 0 1
        (*(DynBigarray.create 1 Bigarray.float32 0.) 0 1;*)
end
end
*)
