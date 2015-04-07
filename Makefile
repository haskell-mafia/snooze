MFLAGS =
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
CABAL_FLAGS =
LIB = lib
DEPS = .cabal-sandbox/.cairn
SUBMODULES = \
	${LIB}/orphanarium/orphanarium-core/orphanarium-core.cabal \
	${LIB}/p/p.cabal

.PHONY: build test-pure test-io test repl repl-test quick quick-io tags

default: repl

${SUBMODULES}:
	git submodule init
	git submodule update

${SANDBOX}: ${SUBMODULES} .gitmodules
	cabal sandbox init
	cabal sandbox add-source lib/orphanarium/orphanarium-core
	cabal sandbox add-source lib/p

${DEPS}: ${SANDBOX} $(wildcard *.cabal) cabal.sandbox.config
	cabal install -j --only-dependencies --enable-tests
	cabal configure --enable-tests ${CABAL_FLAGS}
	touch $@

build: ${DEPS}
	cabal build --ghc-option="-Werror"

test-pure: ${DEPS}
	cabal test test --log=/dev/stdout

test-io: ${DEPS}
	cabal test test-io --log=/dev/stdout

test: test-pure test-io

repl: ${DEPS}
	cabal repl

repl-test: ${DEPS}
	cabal repl test

quick: ${DEPS}
	ghci -DNAPOLEON_DEV_MODE=true -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/test.hs

quick-io: ${DEPS}
	ghci -DNAPOLEON_DEV_MODE=true -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/test-io.hs

tags:
	hasktags -e src test
