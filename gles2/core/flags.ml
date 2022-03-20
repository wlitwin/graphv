module Base = struct
    type t = int
    let no_flags = 0
    let has t ~flag = t land flag > 0
    let or_ t flag = t lor flag
    let ( lor ) = or_
    let remove t ~flag = t land (lnot flag)
end

module type S = module type of Base

