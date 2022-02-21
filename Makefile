.PHONY: all build test clean lint-reuse

all: build test

build:
	stack build $(PACKAGE)

test:
	stack test $(PACKAGE) --test-arguments "$(TEST_ARGUMENTS)"

clean:
	stack clean

lint-reuse:
	reuse lint
