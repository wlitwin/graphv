open Js_of_ocaml

type t = WebGL.renderingContext Js.t
type arg = WebGL.renderingContext Js.t
let create (t : t) : t = t

type enum = int
type blending_factor = WebGL.blendingFactor
type texture_target = WebGL.texTarget
type pixel_format = WebGL.pixelFormat
type pixel_type = WebGL.pixelType
type tex_filter = WebGL.texFilter
type tex_param_filter = WebGL.texFilter WebGL.texParam
type tex_param_wrap = WebGL.wrapMode WebGL.texParam
type tex_param_filter_param = WebGL.texFilter
type tex_param_wrap_param = WebGL.wrapMode
type wrap_mode = WebGL.wrapMode
type pixel_store_param = int WebGL.pixelStoreParam
type enable_cap = WebGL.enableCap
type depth_function = WebGL.depthFunction
type stencil_op = WebGL.stencilOp
type begin_mode = WebGL.beginMode
type cull_face_mode = WebGL.cullFaceMode
type front_face_dir = WebGL.frontFaceDir
type uniform_type = WebGL.uniformType
type buffer_target = WebGL.bufferTarget
type buffer_usage = WebGL.bufferUsage
type error_code = WebGL.errorCode
type texture = WebGL.texture Js.t
type data_type = WebGL.dataType
type buffer = Buffer.Float.t
type 'a uniform_location = 'a WebGL.uniformLocation Js.t
type buffer_id = WebGL.buffer Js.t
type program = WebGL.program Js.t
