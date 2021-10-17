include Graphv_core_lib.Flags.S
val optional : t
val required : t

type pattern = private Optional
                     | Required

val to_pattern : t -> pattern
