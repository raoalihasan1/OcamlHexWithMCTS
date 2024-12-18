(lang dune 3.16)

(name Hex)

(generate_opam_files true)

(source
 (github raoalihasan1/OcamlHexWithMCTS))

(authors "Rao Ali Hasan")

(package
 (name Hex)
 (synopsis "An MCTS AI agent that plays the Hex game out against an opponent")
 (depends ocaml dune core async domainslib)
 (tags
  (topics agent ai game hex mcts)))
