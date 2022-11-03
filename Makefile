.PHONY: test
test:
	nix-shell --run "cabal run haskell-book-reading-group-exercises-test"

.PHONY: performance-chapter04
performance-chapter04:
	cabal v2-run --enable-profiling performance-chapter04 -- +RTS -p -RTS
