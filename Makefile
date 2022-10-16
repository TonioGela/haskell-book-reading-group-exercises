.PHONY: test
test:
	nix-shell --run "cabal run haskell-book-reading-group-exercises-test"