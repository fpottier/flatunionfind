(lang dune 3.0)
(generate_opam_files true)
(package
  (name flatunionfind)
  (authors
    "François Pottier <francois.pottier@inria.fr>"
  )
  (maintainers
    "François Pottier <francois.pottier@inria.fr>"
  )
  (source (uri git+https://github.com/fpottier/flatunionfind.git))
  (homepage https://github.com/fpottier/flatunionfind/)
  (bug_reports https://github.com/fpottier/flatunionfind/issues)
  (synopsis "A union-find data structure inside a vector")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    hachis
    (hector (>= 20241208))
  )
)
