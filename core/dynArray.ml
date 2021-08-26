type 'a t = {
    empty_value : 'a;
    mutable arr : 'a array;
    mutable size : int;
}

let empty t = t.size = 0

let remove t idx =
    for i=idx to t.size-2 do
        t.arr.(i) <- t.arr.(i+1)
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

let get t idx = 
    (*assert (idx >= 0 && idx < t.size);*)
    Array.unsafe_get t.arr idx

let add t value =
    let len = Array.length t.arr in
    if t.size >= len then (
        let new_arr = Array.init 
            (len * 3 / 2) 
            (fun i -> if i < (*t.size*) len then t.arr.(i) else t.empty_value)
        in
        t.arr <- new_arr;
    );
    t.arr.(t.size) <- value;
    t.size <- t.size + 1;
;;

let steal t create =
    let len = Array.length t.arr in
    if t.size >= len then (
        let new_arr = Array.init 
            (len * 3 / 2) 
            (* WARN - We can use len because the array should have been made with init *)
            (fun i -> if i < (*t.size*) len then t.arr.(i) else create())
        in
        t.arr <- new_arr;
    );
    let v = t.arr.(t.size) in
    t.size <- t.size + 1;
    v
;;

let insert (t : 'a t) (idx : int) (value : 'a) : unit =
    add t value;
    for i=t.size-1 downto idx+1 do
        t.arr.(i) <- t.arr.(i - 1);
    done;
    t.arr.(idx) <- value
;;

let [@inline] length t = t.size

let iter (t : 'a t) ~(f : 'a -> unit) : unit =
    let len = t.size-1 in
    for i=0 to len do
        f (Array.unsafe_get t.arr i)
    done

let clear ?(free=false) t = 
    if free then (
        for i=0 to t.size-1 do
            t.arr.(i) <- t.empty_value
        done
    );
    t.size <- 0

let last t =
    t.arr.(t.size-1)
;;

let first t =
    t.arr.(0)
;;

let last_opt t =
    if t.size = 0 then None
    else Some (last t)
;;

let pop_back t =
    t.size <- max (t.size - 1) 0
;;

let unsafe_array t = t.arr
