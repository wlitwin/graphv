(rule
  (target path.ml)
  (deps ../../shared/path.ml)
  (action
    (copy %{deps} %{target}))
  )

(rule
  (target dyn.ml)
  (deps ../../shared/dyn.ml)
  (action
    (copy %{deps} %{target}))
  )

(rule
  (target vertexBuffer.ml)
  (deps ../../shared/vertexBuffer.ml)
  (action
    (copy %{deps} %{target}))
  )
