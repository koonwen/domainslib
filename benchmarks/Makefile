B := ../_build/default/benchmarks/bench.exe
WARMUP := 5
ITER := 5
bench : test_params counter skiplist btree

test_params :
	dune build
	printf "TESTPARAMS -- Warmup = %d, Iterations = %d\n" $(WARMUP) $(ITER)

C_OPS := 10_000_000
counter : counter.ml
	printf "Counter-Sequential\n"
	$(B) counter-sequential --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP)

	printf "Counter-Coarse-Grained\n"
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 1
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 2
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 3
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 4
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 5
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 6
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 7
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 8

	printf "Counter-Batched\n"
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 1
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 2
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 3
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 4
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 5
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 6
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 7
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 8

S_PRESET := 1_000_000
S_INS := 1_000_000
# Need to fix assertion error with search
S_SEARCH := 0
S_SIZE := 0
skiplist : skiplist.ml
	printf "Skiplist-Sequential\n"
	$(B) skiplist-sequential --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP)

	printf "Skiplist-Coarse-Grained\n"
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 1
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 2
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 3
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 4
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 5
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 6
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 7
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 8

	printf "Skiplist-Batched\n"
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

	printf "Skiplist-Lazy\n"
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

B_PRESET := 0
B_OPS := 1_000_000
btree : btree.ml
	printf "Btree-Sequential\n"
	$(B) btree-sequential --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP)

	printf "Btree-Coarse-Grained\n"
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8

	printf "Btree-Batched\n"
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8

btree_batched :
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8

skiplist_lazy :
	printf "Skiplist-Lazy\n"
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

skiplist_explicit :
	printf "Skiplist-explicit\n"
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

btree_batched_validate :
	printf "Btree-batched\n"
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8 -T

skiplist_batched :
	printf "Skiplist-Batched\n"
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

counter_batched :
	printf "Counter-Batched\n"
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 1
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 2
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 3
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 4
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 5
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 6
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 7
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 8