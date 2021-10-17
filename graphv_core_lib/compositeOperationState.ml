type t = {
    src_rgb : BlendFactor.t;
    dst_rgb : BlendFactor.t;
    src_alpha : BlendFactor.t;
    dst_alpha : BlendFactor.t;
}

let of_composite_operation (op : CompositeOperation.t) = 
    let open BlendFactor in
    let sfactor, dfactor =
        match op with
        | Source_over -> One, One_minus_src_alpha
        | Source_in -> Dst_alpha, Zero
        | Source_out -> One_minus_dst_alpha, Zero
        | Atop -> Dst_alpha, One_minus_src_alpha
        | Destination_over -> One_minus_dst_alpha, One
        | Destination_in -> Zero, Src_alpha
        | Destination_out -> Zero, One_minus_src_alpha
        | Destination_atop -> One_minus_dst_alpha, Src_alpha
        | Lighter -> One, One
        | Copy -> One, Zero
        | Xor -> One_minus_dst_alpha, One_minus_src_alpha
    in
    {
        src_rgb = sfactor;
        dst_rgb = dfactor;
        src_alpha = sfactor;
        dst_alpha = dfactor;
    }
