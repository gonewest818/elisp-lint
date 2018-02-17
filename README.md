elisp-lint
==========

Basic linting for Emacs Lisp

[![MELPA](https://melpa.org/packages/elisp-lint-badge.svg)](https://melpa.org/#/elisp-lint)
[![Build Status](https://travis-ci.org/gonewest818/elisp-lint.png?branch=master)](https://travis-ci.org/gonewest818/elisp-lint)

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

### checkdoc ###

Runs checkdoc on the file to enforce standards in documentation.

### fill-column ###

Verifies that no line exceeds the number of columns in `fill-column`.

### indent ###

Verifies that each line is indented according to `emacs-lisp-mode`. Where macros
are defined with special `indent` metadata, use the `lintel-indent-specs` alist
to specify each symbol's required indent.

### indent-character ###

Verifies the indentation is consistently tabs or spaces, according to the value
of `indent-tabs-mode`.

### package-format ###

Calls `package-buffer-info` to validate some file metadata.

### trailing-whitespace ###

Verifies the buffer has no lines with trailing whitespace.

Configuration
-------------

Use a file variable or `.dir-locals.el` to override the variables mentioned
above.

Credits
-------

The initial development of `elisp-lint` is Copyright 2013-2015 Nikolaj
Schumacher. This project was transferred to Neil Okamoto in 2018.

Updates and ongoing development are Copyright 2018 Neil Okamoto and contributors.

Contributing
------------

Pull requests are welcome!
