From ocaml/opam:debian-11-ocaml-5.0

# set workdir
WORKDIR /home/opam/code

COPY --chown=opam ./domainslib.opam .

# install files
RUN opam update && opam upgrade && opam install --deps-only --verbose . -y

COPY --chown=opam ./benchmarks ./benchmarks
COPY --chown=opam ./data ./data
COPY --chown=opam ./lib ./lib
COPY --chown=opam ./multicoretests ./multicoretests
COPY --chown=opam ./test ./test
COPY --chown=opam ./dune-project ./dune-project

RUN eval $(opam env) && dune build @all