(lang dune 3.21)

(name mini-langs)

(generate_opam_files true)

(source
 (github francescoborri/mini-langs))

(authors "Francesco Borri")

(using menhir 3.0)

(package
 (name mini-langs)
 (synopsis
  "Interpreters and a compiler for the final exam of Languages, Compilers and Interpreters at the University of Pisa")
 (description
  "The repository contains the implementation for an interpreter for a simple functional language, miniTyFun, and both an interpreter and a compiler for a simple imperative language, miniImp. The compiler targets a simple RISC-like language, miniRISC, of which an interpreter is also provided.")
 (depends ocaml ocamllex menhir))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
