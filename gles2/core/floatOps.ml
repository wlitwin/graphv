external ( + ) : float -> float -> float = "%addfloat"[@@inline always]
external ( - ) : float -> float -> float = "%subfloat"[@@inline always]
external ( / ) : float -> float -> float = "%divfloat"[@@inline always]
external ( * ) : float -> float -> float = "%mulfloat"[@@inline always]

external [@inline always] ( =. ) : int -> int -> bool = "%eq"
external [@inline always] ( <. ) : int -> int -> bool = "%ltint"
external [@inline always] ( >. ) : int -> int -> bool = "%gtint" 
external [@inline always] ( >=. ) : int -> int -> bool = "%geint" 
external [@inline always] ( <=. ) : int -> int -> bool = "%leint"

external [@inline always] ( < ) : float -> float -> bool = "%ltfloat"
external [@inline always] ( > ) : float -> float -> bool = "%gtfloat" 
external [@inline always] ( >= ) : float -> float -> bool = "%gefloat" 
external [@inline always] ( <= ) : float -> float -> bool = "%lefloat"
external [@inline always] ( = ) : float -> float -> bool = "%eqfloat"

let [@inline always] min (a : float) (b : float) : float =
    if a < b then a else b

let [@inline always] max (a : float) (b : float) : float =
    if a > b then a else b

let [@inline always] imin (a : int) (b : int) : int =
    if a <. b then a else b

let [@inline always] imax (a : int) (b : int) : int =
    if a >. b then a else b

external ( +. ) : int -> int -> int = "%addint"[@@inline always]
external ( -. ) : int -> int -> int = "%subint"[@@inline always]
external ( *. ) : int -> int -> int = "%mulint"[@@inline always]
external ( /. ) : int -> int -> int = "%divint"[@@inline always]

