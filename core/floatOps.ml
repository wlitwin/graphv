external ( + ) : float -> float -> float = "%addfloat"[@@inline always]
external ( - ) : float -> float -> float = "%subfloat"[@@inline always]
external ( / ) : float -> float -> float = "%divfloat"[@@inline always]
external ( * ) : float -> float -> float = "%mulfloat"[@@inline always]

let [@inline always] ( =. ) (a : int) (b : int) : bool = Int.equal a b

let [@inline always] ( <. ) (a : int) (b : int) : bool =
    a < b

let [@inline always] ( >. ) (a : int) (b : int) : bool =
    a > b

let [@inline always] ( >=. ) (a : int) (b : int) : bool =
    a >= b

let [@inline always] ( <=. ) (a : int) (b : int) : bool =
    a <= b

let [@inline always] ( < ) (a : float) (b : float) : bool =
    a < b

let [@inline always] ( > ) (a : float) (b : float) : bool =
    a > b

let [@inline always] ( >= ) (a : float) (b : float) : bool =
    a >= b

let [@inline always] ( <= ) (a : float) (b : float) : bool =
    a <= b

let [@inline always] min (a : float) (b : float) : float =
    if a < b then a else b

let [@inline always] max (a : float) (b : float) : float =
    if a > b then a else b

let [@inline always] imin (a : int) (b : int) : int =
    if a <. b then a else b

let [@inline always] imax (a : int) (b : int) : int =
    if a >. b then a else b

let [@inline always] ( = ) (a : float) (b : float) : bool = Float.equal a b

external ( +. ) : int -> int -> int = "%addint"[@@inline always]
external ( -. ) : int -> int -> int = "%subint"[@@inline always]
external ( *. ) : int -> int -> int = "%mulint"[@@inline always]
external ( /. ) : int -> int -> int = "%divint"[@@inline always]

