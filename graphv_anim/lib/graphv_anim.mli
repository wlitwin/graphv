type repeat = Count of int | Infinite
type direction = Forward | Mirror of direction | Backward
type reason = Done | Canceled
type anim

module Ease = Ease

val create :
    ?delay:float ->
    ?ease:Ease.t ->
    ?complete:(int -> reason -> unit) ->
    ?repeat:repeat ->
    ?repeat_delay:float ->
    ?direction:direction -> float ->
    (float -> unit) ->
    anim

val serial :
    ?delay:float ->
    ?ease:Ease.t ->
    ?complete:(int -> reason -> unit) ->
    ?repeat:repeat ->
    ?repeat_delay:float ->
    ?direction:direction ->
    anim list ->
    anim

val parallel :
    ?delay:float ->
    ?ease:Ease.t ->
    ?complete:(int -> reason -> unit) ->
    ?repeat:repeat ->
    ?repeat_delay:float ->
    ?direction:direction ->
    anim list ->
    anim

module Driver :
sig
  type t
  val create : unit -> t
  val start : t -> anim -> int
  val start_ : t -> anim -> unit
  val is_empty : t -> bool
  val tick : t -> float -> unit
  val cancel : t -> int -> unit
  val cancel_all : t -> unit
  val active_count : t -> int
  val pending_count : t -> int
end

val lerp : float -> float -> float -> float
