all: tests types-check

tests:
	pact test-bro-lottery.repl
	pact test-helpers.repl

types-check:
	pact types-check.repl 2>&1 | grep -v ecko
