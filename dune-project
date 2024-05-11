(lang dune 3.15)

(name raikou)

(generate_opam_files true)

(source
 (github apaaris/raikou))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name raikou)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base stdio ounit2)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
