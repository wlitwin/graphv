(lang dune 2.8)
(generate_opam_files true)
(version 0.2.0)
(source (github wlitwin/graphv))
(authors "Walter Litwinczyk")
(maintainers "walter@litwinczyk.com")
(license MIT)
(documentation https://wlitwin.github.io/docs/graphv/graphv)
(package
  (name graphv_anim)
  (synopsis "A small animation library for graphv")
  (version 0.1.0)
  (depends
    (ocaml (>= 4.08))
    ))

(package
 (name graphv_webgl)
 (synopsis "Full version of the Graphv library based on WebGL1")
 (depends
  (js_of_ocaml-ppx (>= 5.5.0))
  (js_of_ocaml (>= 5.5.0))
   (ocaml (>= 4.08))
   )
 )
(package
 (name graphv_webgl2)
 (synopsis "Full version of the Graphv library based on WebGL2")
 (depends
  (js_of_ocaml-ppx (>= 5.5.0))
  (js_of_ocaml (>= 5.5.0))
   (ocaml (>= 4.08))
   )
 )

(package
 (name graphv_gles2)
 (synopsis "Full version of the Graphv library based on native GLES2")
 (depends
  (stb_truetype (>= 0.6))
   (ocaml (>= 4.08))
   )
 )
(package
 (name graphv_gles3)
 (synopsis "Full version of the Graphv library based on native GLES3")
 (depends
  (stb_truetype (>= 0.6))
   (ocaml (>= 4.08))
   )
 )
