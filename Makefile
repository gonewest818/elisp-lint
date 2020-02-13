export EMACS ?= emacs
export BATCH = --batch -q -l .emacs/init.el

ELS = $(filter-out elisp-lint-autoloads.el,$(wildcard *.el))
TESTS = $(wildcard test/*.el)
OBJECTS = $(ELS:.el=.elc)
BACKUPS = $(ELS:.el=.el~) $(TESTS:.el=.el~)

.PHONY: version lint test clean cleanelpa

.elpa:
	$(EMACS) $(BATCH)
	touch .elpa

version: .elpa
	$(EMACS) $(BATCH) --version

lint: .elpa
	$(EMACS) $(BATCH) -l elisp-lint.el -f elisp-lint-files-batch $(ELS)
	$(EMACS) $(BATCH) -l elisp-lint.el -f elisp-lint-files-batch \
	                  --no-byte-compile \
	                  --no-package-lint \
	                  --no-checkdoc \
	                  --no-check-declare $(TESTS)

test: .elpa
	$(EMACS) $(BATCH) -f buttercup-run-discover

coverage.json: .elpa $(ELS) $(TESTS)
	UNDERCOVER_FORCE=1 $(EMACS) $(BATCH) -f buttercup-run-discover

submit-coverage: coverage.json
	curl -s https://codecov.io/bash | bash -s - -f coverage.json

clean:
	rm -f $(OBJECTS) $(BACKUPS) elisp-lint-autoloads.el* coverage.json

cleanelpa: clean
	rm -rf .emacs/elpa .emacs/quelpa .emacs/.emacs-custom.el* .elpa

