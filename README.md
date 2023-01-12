# Domainslibs Fork - Batching support

This fork introduces support for a technique called "batching" which aims to increase data structure throughput of programs. In programs that run with "batching" support, data structure operations can be processed as a logical "batch" instead of atomic operations which reduces synchronization overhead and opens up more opportunities for optimisation. The goal of this work is to provide the scheduling infrastructure for the community to innovate and build new data structures for OCaml that stand to benefit from batching.

To read more about Batching, this paper introduces the idea https://www.cse.wustl.edu/~kunal/resources/Papers/batcher.pdf

## Preliminary work and test results
### Batched Counter
Simulating standard parallel counters against batched counters (Workload: 1_000_000 increment operations)
```
     num_domains:      1        2        3        4        5        6        7        8   
      chunk_size:      1        1        1	     1  	    1  	   1 	  1        1
                       => Theoretical upper limit of 10_000 operations in a batch

                      (SEQ)
     LockCounter:    240.07   281.09   241.47   263.38   309.24   316.85   308.37   363.86   ms
 LockfreeCounter:    193.72   151.41   104.89   107.33   126.03   142.75   153.77   163.58   ms
  BatchedCounter:    347.68   653.93   547.71   476.24   480.47   507.11   473.90   496.09   ms
 BatchedCounterF:    318.50   444.43   554.21   465.86   546.57   498.28   518.17   515.28   ms
BatchedCounterFF:    196.00   157.55   119.03   124.75   176.78   224.58   262.69   285.167  ms
```
For standard parallel counters (LockCounter & LockfreeCounter), performance degrades as the number of cores increase. BatchedCounters on the other hand have more modal behaviour, showing best performance at 4 cores and then getting worse after. The chunk_size here is a implementation detail of Domainslib `parallel_for` which refers to the number of tasks that will be grouped together to run asynchronously. A chunk_size of 1 means that every function will as it's own asynchronous task

- BatchedCounter: Disjoint BOP operation, sequential chokepoint to create the "batch"
- BatchedCounterF: Reduce sequential chokepoint from having to generate the "batch"
- BatchedCounterFF: Use fast and slow path

However, there is a non-obvious "best" size for a batch.
```
     num_domains:      1        2        3        4        5        6        7        8 
default chunk_size:  10000     62500    41666    31250    25000    20833    17857    15625
 batch upper limit:    1        16	      32       32       64       64       64       64

     LockCounter:     26.26    49.12   138.62    47.46    59.13    60.18    70.14    73.58  ms
 LockfreeCounter:      6.17    34.87    51.01    63.11    76.32   110.79   166.87   215.01  ms
  BatchedCounter:    120.66   420.63   334.80   291.40   342.05   389.58   430.57   439.56  ms
 BatchedCounterF:     99.30   391.14   326.98   264.88   324.47   391.49   414.29   432.59  ms
BatchedCounterFF:      6.22    23.07    11.55    13.91    11.45    14.19    18.26    12.37  ms


       num_domains:      1        2        3        4        5        6        7        8
        chunk_size:     244      244      244      244      244      244      244      244
                        => Theoretical upper limit of 4096 operations in a batch

       num_domains:      1        2        3        4        5        6        7        8   
     LockCounter:     30.68    50.33   128.55    41.42    59.73    65.77    70.72    79.58  ms
 LockfreeCounter:      7.26    32.90    51.11    64.75    82.21   118.31   180.12   222.33  ms
  BatchedCounter:    122.87   263.97   217.34   185.96   221.44   257.60   279.33   285.27  ms
 BatchedCounterF:    107.86   265.55   220.93   179.26   226.05   250.14   262.84   266.55  ms
BatchedCounterFF:      7.51    20.86    22.57    22.78    28.56    42.71    50.02    51.10  ms
```
My testing shows that the upper limit of 4096 operations per batch provides the best throughput for this batched counter. However, this is unlikely to be the best batch size for all batched data structures.

Other statistics
```
Statistics of batch sizes (batch limit 4096)
1 -> 3
4 -> 1
5 -> 2
7 -> 1
12 -> 2
20 -> 1
29 -> 1
65 -> 1
101 -> 1
642 -> 1
1420 -> 1
2874 -> 1
3047 -> 1
3439 -> 1
3508 -> 1
3517 -> 2
3518 -> 3
3519 -> 74
3520 -> 34
3521 -> 1
3523 -> 2
3530 -> 1
3767 -> 1
4267 -> 1
4559 -> 1
4593 -> 1
4663 -> 1
4667 -> 2
4668 -> 1
4669 -> 4
4670 -> 11
4671 -> 61
4672 -> 39

Performance of par_prefix_sums (10_000_000 ops)
    num_domains:      1        2        3        4        5        6        7        8
Par_prefix_sum:    375.19   144.77   124.04   104.43    90.87    83.14    84.19   109.89

Par_prefix_sum With sleep delay to exaggerate parallelism speedup (100_000 ops with 1ms delay)
   num_domains:      1        2        3        4        5        6        7        8
Par_prefix_sum:   5267.82  2634.60  1810.63  1316.54  1069.05   904.28   819.40   655.97

Implicit batching with sleep delay (100_000 ops with 1ms delay)
   num_domains:      1        2        3        4        5        6        7        8
BatchedCounter:   5268.99  2673.35  1844.65  1345.40  1096.41   929.03   830.48   694.64
```

### Batched Skip List
Skip-list sequential (No concurrency control) inserts vs batched inserts
```
Initialized: 1 Million elements
Inserts: 100,000 elements

       num_domains:      2        3        4        5        6        7        8
default chunk_size:    62500    41666    31250    25000    20833    17857    15625
 batch upper limit:     16	    32       32       64       64       64       64

           Seq_ins:     299      284      299      301      298      284      297  ops/ms
       Batched_ins:     346      432      465      563      585      644      627  ops/ms


       num_domains:      2        3        4        5        6        7        8
default chunk_size:     787	   787      787	 787	     787	    787      787  => Theoretical upper limit of 127 operations in a batch

           Seq_ins:     299      284      299      301      298      284      297  ops/ms
       Batched_ins:     346      432      465      563      585      644      627  ops/ms

------------------------------------------------------------------------------------
Initialized: 10 Million elements
Inserts: 100,000 elements

  num_domains:      2        3        4        5        6        7        8
Batched_ins:       156      240      260      409      432      423      522  ops/ms
    Seq_ins:       137      142      153      153      149      153      149  ops/ms


```
Other Statistics (1 Million inserts)
```
batch_size -> bop performed
1          -> 15626
9          -> 1
62         -> 10
63         -> 15615

Performance of parallel insert (1 million preset, 100_000 inserts)
num_domains:      1        2        3        4        5        6        7        8
Batch_ins:       236      279      294      303      301      304      309      309

Performance of parallel insert (1 million preset, 1 million inserts)
num_domains:      1        2        3        4        5        6        7        8
Batch_ins:       459      730      845      886      896      909      914      924
```


Not sure why there is a slight dip in performance using 5 domains?

## Notes
There is an interesting trade-off between the number of parallel operations running and the cost of parallelising tasks. It seems like creating huge batches, tests with batches as big as 15,000 does not beat tests with batches of size 60. This trade-off seems to be measured in the chunk_size calculation of the parallel-for algorithm. However, it does not always seem to be the best choice especially because it is dependent on how fast the batched operations run vs the sequential operations. I also suspect that if we can avoid the sequential bottle neck when we pass around the operation array, we may be able to attain more consistent behaviour

# Domainslib - Nested-parallel programming

Domainslib provides support for nested-parallel programming. Domainslib provides async/await mechanism for spawning parallel tasks and awaiting their results. On top of this mechanism, domainslib provides parallel iteration functions. At its core, domainslib has an efficient implementation of work-stealing queue in order to efficiently share tasks with other domains.

Here is a _sequential_ program that computes nth Fibonacci number using recursion:

```ocaml
(* fib.ml *)
let n = try int_of_string Sys.argv.(1) with _ -> 1

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let main () =
  let r = fib n in
  Printf.printf "fib(%d) = %d\n%!" n r

let _ = main ()
```

We can parallelise this program using Domainslib:

```ocaml
(* fib_par.ml *)
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 1

(* Sequential Fibonacci *)
let rec fib n =
  if n < 2 then 1 else fib (n - 1) + fib (n - 2)

module T = Domainslib.Task

let rec fib_par pool n =
  if n > 20 then begin
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b
  end else
    (* Call sequential Fibonacci if the available work is small *)
    fib n

let main () =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let res = T.run pool (fun _ -> fib_par pool n) in
  T.teardown_pool pool;
  Printf.printf "fib(%d) = %d\n" n res

let _ = main ()
```

The parallel program scales nicely compared to the sequential version. The results presented below were obtained on a 2.3 GHz Quad-Core Intel Core i7 MacBook Pro with 4 cores and 8 hardware threads.

```bash
$ hyperfine './fib.exe 42' './fib_par.exe 2 42' \
            './fib_par.exe 4 42' './fib_par.exe 8 42'
Benchmark 1: ./fib.exe 42
  Time (mean ± sd):     1.217 s ±  0.018 s    [User: 1.203 s, System: 0.004 s]
  Range (min … max):    1.202 s …  1.261 s    10 runs

Benchmark 2: ./fib_par.exe 2 42
  Time (mean ± sd):    628.2 ms ±   2.9 ms    [User: 1243.1 ms, System: 4.9 ms]
  Range (min … max):   625.7 ms … 634.5 ms    10 runs

Benchmark 3: ./fib_par.exe 4 42
  Time (mean ± sd):    337.6 ms ±  23.4 ms    [User: 1321.8 ms, System: 8.4 ms]
  Range (min … max):   318.5 ms … 377.6 ms    10 runs

Benchmark 4: ./fib_par.exe 8 42
  Time (mean ± sd):    250.0 ms ±   9.4 ms    [User: 1877.1 ms, System: 12.6 ms]
  Range (min … max):   242.5 ms … 277.3 ms    11 runs

Summary
  './fib_par2.exe 8 42' ran
    1.35 ± 0.11 times faster than './fib_par.exe 4 42'
    2.51 ± 0.10 times faster than './fib_par.exe 2 42'
    4.87 ± 0.20 times faster than './fib.exe 42'
```

More example programs are available [here](https://github.com/ocaml-multicore/domainslib/tree/master/test).

## Installation

You can install this library using `OPAM`.

```bash
$ opam switch create 5.0.0+trunk --repo=default,alpha=git+https://github.com/kit-ty-kate/opam-alpha-repository.git
$ opam install domainslib
```

## Development

If you are interested in hacking on the implementation, then `opam pin` this repository:

```bash
$ opam switch create 5.0.0+trunk --repo=default,alpha=git+https://github.com/kit-ty-kate/opam-alpha-repository.git
$ git clone https://github.com/ocaml-multicore/domainslib
$ cd domainslib
$ opam pin add domainslib file://`pwd`
```
