(lang dune 2.8)
(name graphv)
(generate_opam_files true)
(version 0.2.0)
(source (github wlitwin/graphv))
(authors "Walter Litwinczyk")
(maintainers "walter@litwinczyk.com")
(license MIT)
(documentation https://wlitwin.github.io/docs/graphv/graphv)
(package
 (name graphv_core_lib)
 (synopsis "Primitives for the Graphv vector graphics library")
 (depends
   (ppx_expect (and (>= v0.14.1) :with-test))
   (ocaml (>= 4.08))
  ))
(package
 (name graphv_font_js)
 (synopsis "Javascript implementation of the font interface for Graphv")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (= 0.2.0))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_webgl_impl (= 0.2.0))
  (graphv_core_lib (= 0.2.0))))
(package
 (name graphv_font)
 (synopsis "Functor for generating the Graphv font library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_core_lib (= 0.2.0))))
(package
 (name graphv_font_stb_truetype)
 (synopsis "STB truetype implementation of the font interface for Graphv")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (stb_truetype (>= 0.6))
  (graphv_font (= 0.2.0))
  (graphv_core_lib (= 0.2.0))))
(package
  (name graphv_core)
  (synopsis "Functor for creating a new Graphv library based on a font render and backend renderer")
  (depends
    (ppx_expect (and (>= v0.14.1) :with-test))
    (graphv_core_lib (= 0.2.0))
    )
  )
(package
 (name graphv_gles2_native_impl)
 (synopsis
  "Native GLES2 implementation of the backend renderer for the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (conf-gles2 (>= 1))
  (graphv_core_lib (= 0.2.0))))
(package
 (name graphv_gles3_native_impl)
 (synopsis
  "Native GLES3 implementation of the backend renderer for the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (conf-gles2 (>= 1))
  (graphv_core_lib (= 0.2.0))))
(package
 (name graphv_webgl_impl)
 (synopsis
  "WebGL implementation of the backend renderer for the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_core_lib (= 0.2.0))))
(package
 (name graphv_webgl2_impl)
 (synopsis
  "WebGL implementation of the backend renderer for the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_core_lib (= 0.2.0))))
(package
 (name graphv_gles2_native)
 (synopsis "Full version of the Graphv library based on native GLES2")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (= 0.2.0))
  (graphv_font_stb_truetype (= 0.2.0))
  (graphv_gles2_native_impl (= 0.2.0))
  (graphv_core (= 0.2.0))
  ))
(package
 (name graphv_gles3_native)
 (synopsis "Full version of the Graphv library based on native GLES2")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (= 0.2.0))
  (graphv_font_stb_truetype (= 0.2.0))
  (graphv_gles3_native_impl (= 0.2.0))
  (graphv_core (= 0.2.0))
  ))
(package
 (name graphv_webgl)
 (synopsis "Full version of the Graphv library based on WebGL")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (= 0.2.0))
  (graphv_font_js (= 0.2.0))
  (graphv_webgl_impl (= 0.2.0))
  (graphv_core (= 0.2.0))))
(package
 (name graphv_webgl2)
 (synopsis "Full version of the Graphv library based on WebGL")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (= 0.2.0))
  (graphv_font_js (= 0.2.0))
  (graphv_webgl2_impl (= 0.2.0))
  (graphv_core (= 0.2.0))))
(package
 (name graphv)
 (synopsis "Top_level graphv package, includes all dependencies")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_gles2_native (= 0.2.0))
  (graphv_webgl (= 0.2.0))
  (graphv_gles3_native (= 0.2.0))
  (graphv_webgl2 (= 0.2.0))
  ))
(package
  (name graphv_anim)
  (synopsis "A small animation library for graphv")
  (version 0.1.0)
  (depends
    (ocaml (>= 4.08))
    ))
