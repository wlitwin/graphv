type float_buffer = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

module UByte = struct
    type t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    let create size =
        let arr =
            Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size
        in
        Bigarray.Array1.fill arr 0;
        arr

    let get : t -> int -> int = Bigarray.Array1.unsafe_get
    let set : t -> int -> int -> unit = Bigarray.Array1.unsafe_set
    let length : t -> int = Bigarray.Array1.dim
    let sub : t -> int -> int -> t = Bigarray.Array1.sub
    let empty : t = create 0
end

external fast_ba_fill : float_buffer -> float -> unit = "fast_ba_fill"[@@noalloc]

module Float = struct
    type t = float_buffer

    let create size =
        let arr =
            Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout size
        in
        Bigarray.Array1.fill arr 0.;
        arr

    let get : t -> int -> float = Bigarray.Array1.unsafe_get
    let set : t -> int -> float -> unit = Bigarray.Array1.unsafe_set
    let length : t -> int  = Bigarray.Array1.dim
    let fill : t -> float -> unit  = (*Bigarray.Array1.fill*) fast_ba_fill
    let blit ~(src : t) ~(s_off : int) ~(dst : t) ~(d_off : int) ~(len : int) : unit = 
        let a = Bigarray.Array1.sub src s_off len in
        let b = Bigarray.Array1.sub dst d_off len in
        Bigarray.Array1.blit a b
end
