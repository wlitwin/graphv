external ( + ) : float -> float -> float = "%addfloat"[@@inline always]
external ( - ) : float -> float -> float = "%subfloat"[@@inline always]
external ( / ) : float -> float -> float = "%divfloat"[@@inline always]
external ( * ) : float -> float -> float = "%mulfloat"[@@inline always]

external [@inline always] ( =. ) : int -> int -> bool = "%equal"
external [@inline always] ( <. ) : int -> int -> bool = "%lessthan"
external [@inline always] ( >. ) : int -> int -> bool = "%greaterthan" 
external [@inline always] ( >=. ) : int -> int -> bool = "%greaterequal" 
external [@inline always] ( <=. ) : int -> int -> bool = "%lessequal"

external [@inline always] ( < ) : float -> float -> bool = "%lessthan"
external [@inline always] ( > ) : float -> float -> bool = "%greaterthan" 
external [@inline always] ( >= ) : float -> float -> bool = "%greaterequal" 
external [@inline always] ( <= ) : float -> float -> bool = "%lessequal"
external [@inline always] ( = ) : float -> float -> bool = "%equal"

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

