module GL = Graphv_gles2_native_impl.Gles2
module GLES2 = Graphv_gles2.Make(GL)
module Stb = Graphv_font_stb_truetype
module Font = Graphv_font.Fontstash.Make(Stb)

include Graphv_core.Make(GLES2)(Font)
