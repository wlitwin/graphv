(rule
  (target fragUniforms.ml)
  (deps ../common/fragUniforms.ml)
  (action
   (copy %{deps} %{target}))
  )

(rule
  (target gles3_shaders.ml)
  (deps ../common/gles3_shaders.ml)
  (action
    (copy %{deps} %{target}))
  )

