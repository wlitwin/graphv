(** This module is for creating new Graphv renderers. It is a functor 
    that takes a render backend and a font backend and produces a new
    rendering library. For concrete implementations see {!module: Graphv_gles2_native}
    for native GLES2 rendering or {!module:Graphv_webgl} for a WebGL/Js_of_ocaml
    based implementation.
*)

module Context : sig 
    module type S = Context.S
end

module Make 
    (Impl : Graphv_core_lib.Impl.S)
    (Font : Graphv_core_lib.Font_impl.S with type data := Impl.Buffer.UByte.t)
    : Context.S
    with type Buffer.UByte.t = Impl.Buffer.UByte.t
    and type Buffer.Float.t = Impl.Buffer.Float.t
    and type arg = Impl.gl
