(lang dune 2.0)
(name ppx_const)
(version 2.0)

(allow_approximate_merlin)
(generate_opam_files true)

(maintainers "Andi McClure <andi.m.mcclure@gmail.com>")
(authors "Andi McClure <andi.m.mcclure@gmail.com>")
(license "Creative Commons Zero")
(source (github mcclure/ppx_const))
(package
  (name ppx_const)
  (synopsis "Compile-time \"if\" statement for conditional inclusion of code.")
  (description "\
This is a ppx extension which adds `if#const` and `match#const` constructs to
OCaml. They behave like normal `if` and `const`, but conditions are evaluated
at compile time and AST sections not selected are excluded from the program
completely. In conjunction with ppx_getenv, this can be used for conditional
compilation of code.
")
  (tags ("syntax"))
  (depends
    (ocaml (>= 4.04.0))
    (ppxlib (>= 0.9.0))
    (ounit2 :with-test)
    (ppx_getenv (and :with-test (>= 2.0)))
    (odoc :with-doc)))
