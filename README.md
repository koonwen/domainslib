# OBatcher Domainslibs Fork Artefact

This library implements an experimental research fork of Multicore
OCaml's standard Domainslib library that implements batch-parallel 
data structures.

- Domainslib is available here: https://github.com/ocaml-multicore/domainslib
  - Our development is built on top of a pending PR to domainslib that
    adds support for creating empty promises, as exposed by the
    function `promise` in `lib/task.ml`, line 40.

- The implementation of OBatcher is under `lib/batcher.ml` which
  provides the `Make` functor it provides to convert explicitly
  batched data-structures to direct style.
  
- The directory `data/` provides the implementation of OBatcher's
  batch-parallel B-tree (in `data/btree.ml`) and its batch-parallel
  Skiplist (in `data/skiplist.ml`). 

- The directory `multicoretests/src/` contains our linearisability
  tests for the skiplist (in
  `multicoretests/src/skiplist/lin_tests.ml`) and btree (in
  `multicoretests/src/btree/lin_tests.ml`) built on top of OCaml
  multicoretests' (https://github.com/ocaml-multicore/multicoretests)
  testing harness.

- The `benchmarks` directory includes the experimental setup for any
  evaluations presented in the paper. The jupyter notebook
  `paper-artefact.ipynb` can be used to reproduce our results.
  
  - We do not package the `datalog` library in this artefact as it is
    available on opam. `benchmarks/datalog_bench.ml` describes our
    coarse-grained and batched wrappers around this library.

Note: For the jupyter notebook, you will need `ipympl` installed, via `pip install ipympl`.

This fork introduces support for a technique called "batching" which
aims to increase data structure throughput of programs. In programs
that run with "batching" support, data structure operations can be
processed as a logical "batch" instead of atomic operations which
reduces synchronization overhead and opens up more opportunities for
optimisation. The goal of this work is to provide the scheduling
infrastructure for the community to innovate and build new data
structures for OCaml that stand to benefit from batching.

The main contributions of our work include:
1. A portable Batcher which makes turning explicitly batched data
   structures to their implcitly batched versions cheap.
2. A Batcher that allows multiple batched data structures in the same
   program
3. Investigation of the performance characteristics of implicitly
   batched data structures

To read more about batching, this paper introduces the idea https://www.cse.wustl.edu/~kunal/resources/Papers/batcher.pdf


The rest of this README will detail how to setup OBatcher locally and
run the experiments.


## Setting up
Naturally, as OBatcher is an OCaml library, you will need to have OCaml and opam, the OCaml package manager installed.

To build OBatcher locally, you must have a 5.0.0 OCaml Switch:

```shell
# create switch if you don't already have it
$ opam switch create 5.0.0
# switch to 5.0.0
$ opam swicth 5.0.0
```

Install the dependencies using the opam file we provide:
```
opam install --deps-only .
```

Then you will be able to build OBatcher using dune:
```
opam exec -- dune build @all
```

We provide an executable in `benchmarks/bench.ml` that will run the
benchmarks when supplied with appropriate parameters, to be discussed
in the `Running Experiments` section.

## Running Experiments

To run the experiments, we build an executable that handles
initialising and timing executions of data-structures on random
workloads, `benchmarks/bench.ml`. 

This executable can be run manually to obtain the timings for a
particular configuration, or we provide a jupyter-notebook which
automates the process of collecting graphs of timing and throughput as
the parameters are varied.

### Running Manually

To run it yourself manually, `cd` into `benchmarks`, and run:

```
opam exec -- dune exec -- ./bench.exe  --help
```

This utility has a self-documenting API with manual pages, so use `--help` to navigate which parameters to use:
```
NAME
       bench.exe - Run benchmarks and print result times.

SYNOPSIS
       bench.exe COMMAND …

COMMANDS
       btree-batched [OPTION]…


       btree-coarse-grained [OPTION]…


       btree-explicitly-batched [OPTION]…

       ...
```

The first argument must be a data-structure, and the subsequent
arguments, which vary based on the data-structure being evaluated, can
be viewed by passing `--help`:
```
opam exec -- dune exec -- ./bench.exe btree-batched --help
```

```
NAME
       bench.exe-btree-batched

SYNOPSIS
       bench.exe btree-batched [OPTION]…

OPTIONS
       --branching-factor=VAL
           Branching factor of tree

       -c VAL, --count=VAL (required)
           Number of operations to run

       -D VAL, --no-domains=VAL
           Number of domains to use

       -i VAL, --no-iter=VAL (required)
           Number of iterations to run
       ...
```


### Using the Jupyter Notebook
We provide a Jupyter Notebook that can be used to recreate our experimental results. 

First, you must have jupyterlab installed:
```
pip install jupyterlab
```


To use the Jupyter notebook, start jupyternotebook from the root of this project:
```
jupyter-lab --port=8889
```

Navigate to `benchmarks/paper-artefact.ipynb` to run our experiments.

We make use of a `utils` library (found in `benchmarks/utils.py`) to
define some helpers for running benchmarks. The examples in the
notebook and the markdown should be give an idea of how to use the
helpers.

Of note:

- `utils.plot_results` -- given a list of results from a performance
  evaluation, plots a graph of times

- `utils.plot_throughput_results` -- given a list of results from a
  performance evaluation, plots a graph of throughput
