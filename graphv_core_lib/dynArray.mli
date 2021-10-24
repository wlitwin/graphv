type 'a t
val create : int -> 'a -> 'a t
val init : int -> (unit -> 'a) -> 'a t
val copy : 'a t -> ('a -> 'a) -> 'a t
val insert : 'a t -> int -> 'a -> unit
val remove : 'a t -> int -> unit
val get : 'a t -> int -> 'a
val add : 'a t -> 'a -> unit
val steal : 'a t -> (unit -> 'a) -> 'a
val length : 'a t -> int
val clear : ?free:bool -> 'a t -> unit
val first : 'a t -> 'a
val last : 'a t -> 'a
val last_opt : 'a t -> 'a option
val pop_back : 'a t -> unit
val append_steal : dst:'a t -> src:'a t -> (unit -> 'a) -> unit
val unsafe_array : 'a t -> 'a array
val empty : 'a t -> bool
