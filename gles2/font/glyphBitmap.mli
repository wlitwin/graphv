include Flags.S

val optional : t[@@immediate]
val required : t[@@immediate]

type pattern = private Optional
                     | Required

val to_pattern : t -> pattern
