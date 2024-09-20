open Js_of_ocaml

module UByte = struct
    type t = Typed_array.uint8Array Js.t

    let set = Typed_array.set
    let get = Typed_array.unsafe_get
    let [@inline always] length (t : t) : int = t##.length
    let [@inline always] sub (t : t) (start : int) (len : int) : t =
        t##subarray start len

    let create size =
        new%js Typed_array.uint8Array size

    let empty = create 0
end

module Float = struct
    type t = Typed_array.float32Array Js.t

    let set (x : t) (i : int) (v : float) : unit =
      Typed_array.set x i (Js.float v)
    let get (x : t) (i : int) : float = Js.to_float (Typed_array.unsafe_get x i)
    let [@inline always] length (t : t) : int = t##.length

    let zero (t : t) =
        let len : int = Js.Unsafe.get t "length" - 1 in
        for i=0 to len do
            Typed_array.set t i (Js.float 0.)
        done

    let blit ~(src : t) ~(s_off : int) ~(dst : t) ~(d_off : int) ~(len : int) : unit =
        let len : int = len-1 in
        for i=0 to len do
            Typed_array.set dst (d_off+i) (Typed_array.unsafe_get src (s_off+i))
        done
    ;;

    let create size = new%js Typed_array.float32Array size
end
