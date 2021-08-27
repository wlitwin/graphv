(lang dune 2.8)
(name graphv)
(generate_opam_files true)
(version 0.1.0)
(source (github wlitwin/graphv))
(authors "Walter Litwinczyk")
(maintainers "walter@litwinczyk.com")
(license MIT)
(documentation https://wlitwin.github.io/docs/graphv/graphv)
(package
 (name graphv_core_lib)
 (synopsis "Primitives for the Graphv vector graphics library")
 (depends
  ))
(package
 (name graphv_font_js)
 (synopsis "Javascript implementation of the font interface for Graphv")
 (depends
  (graphv_font (>= 0.1))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_webgl_impl (>= 0.1))
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv_font)
 (synopsis "Functor for generating the Graphv font library")
 (depends
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv_font_stb_truetype)
 (synopsis "STB truetype implementation of the font interface for Graphv")
 (depends
  (stb_truetype (>= 0.6))
  (graphv_font (>= 0.1))
  (graphv_core_lib (>= 0.1))))
(package
  (name graphv_core)
  (synopsis "Functor for creating a new Graphv library based on a font render and backend renderer")
  (depends
    (graphv_core_lib (>= 0.1))
    )
  )
(package
 (name graphv_gles2_native_impl)
 (synopsis
  "Native GLES2 implementation of the backend renderer for the Graphv library")
 (depends
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv_webgl_impl)
 (synopsis
  "WebGL implementation of the backend renderer for the Graphv library")
 (depends
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv_gles2)
 (synopsis "Functor for creating a Graphv renderer based on GLES2")
 (depends
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv_gles2_native)
 (synopsis "Full version of the Graphv library based on native GLES2")
 (depends
  (graphv_font (>= 0.1))
  (graphv_font_stb_truetype (>= 0.1))
  (graphv_gles2_native_impl (>= 0.1))
  (graphv_gles2 (>= 0.1))
  (graphv_core (>= 0.1))
  ))
(package
 (name graphv_webgl)
 (synopsis "Full version of the Graphv library based on WebGL")
 (depends
  (graphv_font (>= 0.1))
  (graphv_font_js (>= 0.1))
  (graphv_webgl_impl (>= 0.1))
  (graphv_gles2 (>= 0.1))
  (graphv_core (>= 0.1))))
(package
 (name graphv)
 (synopsis "Top_level graphv package, includes all dependencies")
 (depends
  (graphv_gles2_native (>= 0.1))
  (graphv_webgl (>= 0.1))
  ))
