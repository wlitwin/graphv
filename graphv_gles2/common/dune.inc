(rule
  (target fragUniforms.ml)
  (deps ../common/fragUniforms.ml)
  (action
   (copy %{deps} %{target}))
  )

(rule
  (target gles2_shaders.ml)
  (deps ../common/gles2_shaders.ml)
  (action
    (copy %{deps} %{target}))
  )

