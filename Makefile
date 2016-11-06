
build:
	stack build

test:
	stack test

test-watch:
	stack test --file-watch --test-arguments "--color"

.PHONY: release test loc clean