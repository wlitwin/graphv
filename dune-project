(lang dune 2.8)
(name graphv)

(generate_opam_files true)

(version 0.1.0)

(source (github wlitwin/graphv))
(authors "Walter Litwinczyk")
(maintainers "walter@litwinczyk.com")

(package
  (name graphv-core-lib)
  (synopsis "Primitives for the Graphv vector graphics library")
  (depends
    (ocaml (>= 4.08))
    )
)

(package
  (name graphv-font.js)
  (synopsis "Javascript implementation of the font interface for Graphv")
  (depends
    (js_of_ocaml (>= 3.9))
    (graphv_core_lib (>= 0.1))
    (graphv_font (>= 0.1))
    (graphv_webgl_impl (>= 0.1))
    )
  )

(package
  (name graphv-font)
  (synopsis "Functor for generating the Graphv font library")
  (depends
    (graphv_core_lib (>= 0.1))
    )
  )

(package
  (name graphv-font.stb-truetype)
  (synopsis "STB truetype implementation of the font interface for Graphv")
  (depends
    (graphv_core_lib (>= 0.1))
    (stb_truetype (>= 0.6))
    (graphv_font (>= 0.1))
    )
  )

(package
  (name graphv-gles2.native-impl)
  (synopsis "Native GLES2 implementation of the backend renderer for the Graphv library")
  (depends
    (graphv_core_lib (>= 0.1))
    (conf-gles2 (>= 1))
    )
  )

(package
  (name graphv-gles2.webgl-impl)
  (synopsis "WebGL implementation of the backend renderer for the Graphv library")
  (depends
    (graphv_core_lib (>= 0.1))
    (js_of_ocaml (>= 3.9))
    )
  )

(package
  (name graphv-gles2)
  (synopsis "Functor for creating a Graphv renderer based on GLES2")
  (depends
    (graphv_core_lib (>= 0.1))
    )
  )

(package
  (name graphv)
  (synopsis "Top-level Graphv package")
  )

(package
  (name graphv.gles2-native)
  (synopsis "Native version of the Graphv library based on GLES2")
  (depends
    (graphv_font (>= 0.1))
    (graphv_font_stb_truetype (>= 0.1))
    (graphv_gles2_native_impl (>= 0.1))
    (graphv_gles2 (>= 0.1))
    (graphv_core (>= 0.1))
    )
  )

(package
  (name graphv.webgl)
  (synopsis "WebGL version of the Graphv library")
  (depends
    (graphv_font (>= 0.1))
    (graphv_font_js (>= 0.1))
    (graphv_webgl_impl (>= 0.1))
    (graphv_gles2 (>= 0.1))
    (graphv_core (>= 0.1))
    )
  )
