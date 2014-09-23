elisp-lint
==========

Basic linting for Emacs Lisp

[![Build Status](https://travis-ci.org/nschum/elisp-lint.png?branch=master)](https://travis-ci.org/nschum/elisp-lint)

This is a tool for finding certain problems in Emacs Lisp files. Use it on the command line like this:

    emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el

You can disable individual checks, by passing flags on the command line:

    emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch --no-indent *.el

Alternatively, you can disable checks using file variables or the following .dir.locals file:

    ((nil . ((elisp-lint-ignored-validators . ("fill-column")))))

Validators
----------

### byte-compile ###

Byte-compiles the file with all warnings enabled.

### package-format ###

Calls `package-buffer-info` to validate some file metadata.

### indent ###

Verifies that each line is indented according to emacs-lisp-mode.

### fill-column ###

Verifies that no line exceeds the number of columns in fill-column. Use a file variable or a .dir.locals file to override the value.

Pull requests for further tests are welcome.
