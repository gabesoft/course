
build:
	stack build

test:
	stack test

test-watch:
	stack test --file-watch --test-arguments "--color"

chat: build
	stack exec network-chat

.PHONY: release test loc clean