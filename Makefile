.PHONY: build
build:
	cabal build

.PHONY: test
test:
	cabal test --test-show-details=direct

.PHONY: watch
watch:
	watchman-make -p '**/*.hs' -t test