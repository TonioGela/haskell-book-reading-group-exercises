.PHONY: build
build:
	cabal build

.PHONY: test
test:
	cabal test --test-show-details=direct