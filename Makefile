YAML    = stack.yaml
TESTS   = $(wildcard examples/*.cp)
ERRORS  = $(wildcard errors/*.cp)
DEST    =
STACK   = stack --stack-yaml $(YAML)
NULL    =
SOURCES = \
  Atoms.hs \
  Checker.hs \
  Common.hs \
  Exceptions.hs \
  Instrumenter.hs \
  Lexer.x \
  Main.hs \
  Measure.hs \
  Parser.y \
  Process.hs \
  Render.hs \
  Resolver.hs \
  Solver.hs \
  Type.hs \
  $(NULL)
EXTRA_SOURCES = \
  ChangeLog.md \
  errors \
  examples \
  FreeCP.cabal \
  LICENSE \
  LICENSE.hs \
  Makefile \
  README.md \
  Setup.hs \
  stack.yaml \
  $(NULL)

all:
	@$(STACK) build

watch:
	@$(STACK) build --file-watch

install:
	@$(STACK) install

dist: $(SOURCES:%=src/%) $(EXTRA_SOURCES) $(TESTS) $(ERRORS)
	@tar cvfz FreeCP.tar.gz $^

sync:
	@make -C html
	@scp html/*.* $(DEST)
	@scp dist/*.tar.gz $(DEST)

info:
	@$(STACK) exec happy -- -i src/Parser.y

%.check_ok:
	@stack run $(@:%.check_ok=%) || echo

check_examples:
	@echo
	@echo "SCRIPTS THAT MUST PASS"
	@echo "–————————————————————–"
	@for i in $(TESTS); do make -s $$i.check_ok; done

%.check:
	@stack run $(@:%.check=%) || echo

check_errors:
	@echo
	@echo "SCRIPTS THAT MUST FAIL"
	@echo "–————————————————————–"
	@for i in $(ERRORS); do make -s $$i.check; done

update_license:
	for hs in `find src -name "*.hs"`; do \
		TEMP=`mktemp`; \
		cp LICENSE.hs $$TEMP; \
		tail -n +20 <$$hs >>$$TEMP; \
		mv $$TEMP $$hs; \
	done

check: check_examples check_errors
	@echo

.PHONY: dist clean check check_examples check_errors docs

clean:
	@$(STACK) clean
	@rm -f `$(STACK) path --local-bin`/faircheck
