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
 (name graphv-core-lib)
 (synopsis "Primitives for the Graphv vector graphics library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (and (>= 0.1) :with-test))
  (ocaml (>= 4.08))))
(package
 (name graphv-font-js)
 (synopsis "Javascript implementation of the font interface for Graphv")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (js_of_ocaml (>= 3.9))
  (graphv_core_lib (and :with-test (>= 0.1)))
  (graphv_font (and :with-test (>= 0.1)))
  (graphv_webgl_impl (>= 0.1))))
(package
 (name graphv-font)
 (synopsis "Functor for generating the Graphv font library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (stb_truetype (>= 0.6))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_webgl_impl (>= 0))
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv-font-stb-truetype)
 (synopsis "STB truetype implementation of the font interface for Graphv")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_core_lib (and :with-test (>= 0.1)))
  (stb_truetype (>= 0.6))
  (graphv_font (and :with-test (>= 0.1)))))
(package
 (name graphv-gles2-native-impl)
 (synopsis
  "Native GLES2 implementation of the backend renderer for the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (and (>= 0.1) :with-test))
  (graphv_core_lib (and :with-test (>= 0.1)))
  (conf-gles2 (>= 1))))
(package
 (name graphv-gles2-webgl-impl)
 (synopsis
  "WebGL implementation of the backend renderer for the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (and (>= 0.1) :with-test))
  (graphv_core_lib (and :with-test (>= 0.1)))
  (js_of_ocaml (>= 3.9))))
(package
 (name graphv-gles2)
 (synopsis "Functor for creating a Graphv renderer based on GLES2")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_font (and (>= 0.1) :with-test))
  (ppx_jane (>= v0.14.0))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))
  (graphv_core_lib (>= 0.1))))
(package
 (name graphv-gles2-native)
 (synopsis "Native version of the Graphv library based on GLES2")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_core_lib (and (>= 0.1) :with-test))
  (graphv_font (and :with-test (>= 0.1)))
  (graphv_font_stb_truetype (>= 0.1))
  (graphv_gles2_native_impl (>= 0.1))
  (graphv_gles2 (>= 0.1))
  (graphv_core (>= 0.1))))
(package
 (name graphv-webgl)
 (synopsis "WebGL version of the Graphv library")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (graphv_core_lib (and (>= 0.1) :with-test))
  (graphv_font (and :with-test (>= 0.1)))
  (graphv_font_js (>= 0.1))
  (graphv_webgl_impl (>= 0.1))
  (graphv_gles2 (>= 0.1))
  (graphv_core (>= 0.1))))
(package
 (name graphv)
 (synopsis "Top-level graphv package, includes all dependencies")
 (depends
  (ppx_expect (and (>= v0.14.1) :with-test))
  (ppx_jane (>= v0.14.0))
  (js_of_ocaml-ppx (>= 3.9.0))
  (graphv_webgl_impl (>= 0.1))
  (graphv_gles2_native_impl (>= 0.1))
  (graphv_gles2 (>= 0.1))
  (graphv_font_stb_truetype (>= 0.1))
  (graphv_font_js (>= 0.1))
  (graphv_font (>= 0.1))
  (graphv_core_lib (>= 0.1))
  (dune (>= 2.9.0))
  (ocaml (>= 4.08))
  (graphv.webgl (>= 0.1))
  (graphv.gles2-native (>= 0.1))))
