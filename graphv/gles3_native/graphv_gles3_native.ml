module GL = Graphv_gles3_native_impl
module Stb = Graphv_font_stb_truetype
module Font = Graphv_font.Fontstash.Make(Stb)

include Graphv_core.Make(GL)(Font)
