type 'a t = {
    empty_value : 'a;
    mutable arr : 'a array;
    mutable size : int;
}

let [@inline always] empty t = t.size = 0

let remove t idx =
    for i=idx to t.size-2 do
        Array.unsafe_set t.arr i (Array.unsafe_get t.arr (i+1))
    done;
    t.size <- max (t.size - 1) 0;
;;

let create cap value = {
    empty_value = value;
    arr = Array.make cap value;
    size = 0;
}

let init cap f = {
    empty_value = f();
    arr = Array.init cap (fun _ -> f());
    size = 0;
}

let [@inline always] get t idx =
    (*assert (idx >= 0 && idx < t.size);*)
    Array.unsafe_get t.arr idx

let copy (t : 'a t) (copy : 'a -> 'a) : 'a t =
    let arr = Array.init (Array.length t.arr) (fun idx ->
        copy (Array.unsafe_get t.arr idx)
    ) in
    {
        empty_value = t.empty_value;
        arr;
        size = t.size;
    }
;;

let [@cold] resize t =
    let len = Array.length t.arr in
    let new_arr = Array.init
        (len * 3 / 2)
        (fun i -> if i < (*t.size*) len then Array.unsafe_get t.arr i else t.empty_value)
    in
    t.arr <- new_arr;
;;

let add t value =
    let len = Array.length t.arr in
    if t.size >= len then (
      (resize[@cold]) t
    );
    Array.unsafe_set t.arr t.size value;
    t.size <- t.size + 1;
;;

let append_steal ~dst ~src create =
    let d_len = Array.length dst.arr in
    if dst.size + src.size >= d_len then (
        let new_arr = Array.init
            (d_len * 3 / 2)
            (fun i ->
                if i < dst.size then (
                    Array.unsafe_get dst.arr i
                ) else if (i - dst.size) < src.size then (
                    Array.unsafe_get src.arr (i - dst.size)
                ) else (
                    create()
                )
            )
        in
        dst.arr <- new_arr;
    ) else (
        for i=dst.size to (dst.size+src.size) do
            Array.unsafe_set dst.arr i (Array.unsafe_get src.arr (i-dst.size))
        done;
    );
    dst.size <- dst.size + src.size
;;

let steal t create =
    let len = Array.length t.arr in
    if t.size >= len then (
        let new_arr = Array.init
            (len * 3 / 2)
            (* WARN - We can use len because the array should have been made with init *)
            (fun i -> if i < (*t.size*) len then Array.unsafe_get t.arr i else create())
        in
        t.arr <- new_arr;
    );
    let v = Array.unsafe_get t.arr t.size in
    t.size <- t.size + 1;
    v
;;

let insert (t : 'a t) (idx : int) (value : 'a) : unit =
    add t value;
    for i=t.size-1 downto idx+1 do
        Array.unsafe_set t.arr i (Array.unsafe_get t.arr (i - 1));
    done;
    Array.unsafe_set t.arr idx value
;;

let [@inline] length t = t.size

let clear ?(free=false) t =
    if free then (
        for i=0 to t.size-1 do
            Array.unsafe_set t.arr i t.empty_value
        done
    );
    t.size <- 0

let [@inline always] last t =
    Array.unsafe_get t.arr (t.size-1)
;;

let [@inline always] first t =
    Array.unsafe_get t.arr 0
;;

let [@inline always] last_opt t =
    if t.size = 0 then None
    else Some (last t)
;;

let [@inline always] pop_back t =
    t.size <- max (t.size - 1) 0
;;

let [@inline always] unsafe_array t = t.arr
