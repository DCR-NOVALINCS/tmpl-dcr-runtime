(lang dune 3.14)

(name tmpl_dcr)

(generate_opam_files true)

(source
 (github DCR-NOVALINCS/tmpl-dcr-runtime))

(authors "bfibraga")

; (maintainers "Maintainer Name")

(license
 "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License")

(documentation https://url/to/documentation)

(package
 (name tmpl_dcr)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
