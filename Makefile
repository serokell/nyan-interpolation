# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

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
