;;; elisp-lint.el --- basic linting for Emacs Lisp
;;
;; Copyright (C) 2013-2015 Nikolaj Schumacher
;; Copyright (C) 2018 Neil Okamoto
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>,
;; Author: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Version: 0.3
;; Keywords: lisp, maint, tools
;; Package-Requires: ((emacs "24"))
;; URL: http://github.com/gonewest818/elisp-lint/
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a tool for finding certain problems in Emacs Lisp files.  Use it on
;; the command line like this:
;;
;; $(EMACS) -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el
;;
;; You can disable individual checks by passing flags on the command line:
;;
;; $(EMACS) -Q --batch -l elisp-lint.el -f elisp-lint-files-batch \
;;          --no-indent *.el
;;
;; Alternatively, you can disable checks using file variables or the following
;; .dir-locals.el file:
;;
;; ((emacs-lisp-mode . ((elisp-lint-ignored-validators . ("fill-column")))))
;;
;; For a full list of validators, see `elisp-lint-file-validators' and
;; `elisp-lint-buffer-validators'.
;;
;;; Change Log:
;;
;; * Version 0.3 (MELPA)
;;    - Emacs 23 support is deprecated [#13]
;;    - Adopt CircleCI and drop Travis CI [#9] [#14]
;;    - Add check-declare validator [#16]
;;    - Generate autoloads before bytecompile [#8]
;; * Version 0.2 (MELPA Stable - Feb 2018)
;;    - Project transferred to new maintainer
;;    - Whitespace check permits page-delimiter (^L)
;;    - Indentation check prints the diff to console
;;    - User can specify indent specs to tell the checker about macros
;;    - Added checkdoc (available only Emacs 25 and newer)
;;    - Cleared up the console output for easier reading in CI
;;    - Expand Travis CI test matrix to include Emacs 25 and 26
;; * Version 0.1 (2015)
;;    - Basic linting functionality implemented
;;
;;; Code:

(require 'bytecomp)
(require 'check-declare)
(require 'checkdoc nil t)
(require 'package nil t)

(declare-function package-buffer-info "package" t)

(defconst elisp-lint-file-validators
  (nconc '("byte-compile" "check-declare")
         (when (fboundp 'checkdoc-file) '("checkdoc"))))

(defconst elisp-lint-buffer-validators
  '("package-format" "indent" "indent-character" "fill-column"
    "trailing-whitespace"))

(defvar elisp-lint-ignored-validators nil
  "List of validators that should not be run.")
(put 'elisp-lint-ignored-validators 'safe-local-variable 'listp)

(defvar elisp-lint-indent-specs nil
  "Alist of symbols and their indent specifiers.
The property 'lisp-indent-function will be set accordingly on
each of the provided symbols prior to running the indentation
check.  Caller can set this variable as needed on the command
line or in \".dir-locals.el\".  The alist should take the form
`((symbol1 . spec1) (symbol2 . spec2) ...)' where the specs are
identical to the `indent' declarations in defmacro.")
(put 'elisp-lint-indent-specs 'safe-local-variable 'listp)

(defmacro elisp-lint--protect (&rest body)
  "Handle errors raised in BODY."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error (message "%s" (error-message-string err)) nil)))

(defmacro elisp-lint--run (validator &rest args)
  "Run the VALIDATOR with ARGS."
  `(or (member ,validator elisp-lint-ignored-validators)
       (progn
         (message "* Run %s" ,validator)
         (elisp-lint--protect (funcall
                               (intern (concat "elisp-lint--" ,validator))
                               ,@args)))))

(defun elisp-lint--amend-ignored-validators-from-command-line ()
  "Parse command line and find flags to disable specific validators."
  (while (string-match "^--no-\\([a-z-]*\\)" (car command-line-args-left))
    (add-to-list 'elisp-lint-ignored-validators
                 (match-string 1 (pop command-line-args-left)))))

;;; Validators

(defvar elisp-lint--autoloads-filename nil
  "The autoloads file for this package.")

(defun elisp-lint--generate-autoloads ()
  "Generate autoloads and set `elisp-lint--autoloads-filename`.
Assume `default-directory` name is also the package name,
e.g. for this package it will be \"elisp-lint-autoloads.el\"."
  (let* ((dir (directory-file-name default-directory))
         (prefix (file-name-nondirectory dir))
         (pkg (intern prefix))
         (load-prefer-newer t))
    (package-generate-autoloads pkg dir)
    (setq elisp-lint--autoloads-filename (format "%s-autoloads.el" prefix))))

(defun elisp-lint--byte-compile (file)
  "Byte-compile FILE with warnings enabled.
Return nil if errors were found."
  (let ((byte-compile-error-on-warn t)
        (byte-compile-warnings t))
    (unless elisp-lint--autoloads-filename
      (elisp-lint--generate-autoloads))
    (load-file elisp-lint--autoloads-filename)
    (byte-compile-file file)))

(defun elisp-lint--check-declare (file)
  "Validate `declare-function` statements in FILE."
  (let ((errlist (check-declare-file file)))
    (or (null errlist)
        (error "Check-declare failed"))))

;; Checkdoc is available only Emacs 25 or newer
(when (fboundp 'checkdoc-file)
  (defun elisp-lint--checkdoc (file)
    "Run checkdoc on FILE and print the results.
Return nil if errors were found, else t."
    (let* ((msgbuf (get-buffer "*Messages*"))
           (tick (buffer-modified-tick msgbuf)))
      (checkdoc-file file)
      (or (equal tick (buffer-modified-tick msgbuf))
          (error "Checkdoc failed")))))

(defun elisp-lint--package-format ()
  "Call `package-buffer-info' to validate package metadata."
  (or (null (require 'package nil t))
      (package-buffer-info)))

(defun elisp-lint--indent ()
  "Confirm buffer indentation is consistent with `emacs-lisp-mode'.
Use `indent-region' to format the entire buffer, and compare the
results to the filesystem.  Print diffs if there are any
discrepancies.  Prior to indenting the buffer, apply the settings
provided in `elisp-lint-indent-specs' to configure specific
symbols (typically macros) that require special handling."
  (dolist (s elisp-lint-indent-specs)
    (put (car s) 'lisp-indent-function (cdr s)))
  (let ((tick (buffer-modified-tick)))
    (indent-region (point-min) (point-max))
    (or (equal tick (buffer-modified-tick))
        (progn
          (diff-buffer-with-file)
          (with-current-buffer "*Diff*"
            (message "%s" (buffer-string))
            (kill-buffer))
          (error "Indentation is incorrect")))))

(defun elisp-lint--indent-character ()
  "Verify buffer indentation is consistent with `indent-tabs-mode`.
Use a file variable or \".dir-locals.el\" to override the default value."
  (let ((lines nil)
        (re (if indent-tabs-mode
                (elisp-lint--not-tab-regular-expression)
              "^\t"))
        (msg (if indent-tabs-mode
                 "spaces instead of tabs"
               "tabs instead of spaces")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (push (count-lines (point-min) (point)) lines)))
    (or (null lines)
        (error "Lines indented with %s: %s" msg
               (elisp-lint--join-lines lines)))))

(defun elisp-lint--not-tab-regular-expression ()
  "Regex to match a string of spaces with a length of `tab-width`."
  (concat "^" (make-string tab-width ? )))

(defun elisp-lint--join-lines (line-numbers)
  "Convert LINE-NUMBERS list into a comma delimited string."
  (mapconcat (lambda (i) (format "#%d" i)) (sort line-numbers '<) ", "))

(defun elisp-lint--fill-column ()
  "Confirm buffer has no lines exceeding `fill-column` in length.
Use a file variable or \".dir-locals.el\" to override the default value."
  (save-excursion
    (let ((line-number 1)
          (too-long-lines nil))
      (goto-char (point-min))
      (while (not (eobp))
        (goto-char (point-at-eol))
        (when (> (current-column) fill-column)
          (push line-number too-long-lines))
        (setq line-number (1+ line-number))
        (forward-line 1))
      (or (null too-long-lines)
          (error "Lines longer than %d characters: %s"
                 fill-column (elisp-lint--join-lines too-long-lines))))))

(defun elisp-lint--trailing-whitespace ()
  "Confirm buffer has no line with trailing whitespace.
Allow `page-delimiter` if it is alone on a line."
  (save-excursion
    (let ((lines nil))
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]+$" nil t)
        (unless (string-match-p
                 (concat page-delimiter "$") ; allow a solo page-delimiter
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
          (push (count-lines (point-min) (point)) lines)))
      (or (null lines)
          (error "Line numbers with trailing whitespace: %s"
                 (elisp-lint--join-lines (sort lines '<)))))))

;;; Linting

(defun elisp-lint-file (file)
  "Run validators on FILE."
  (with-temp-buffer
    (find-file file)
    (when elisp-lint-ignored-validators
      (message "** Ignoring validators: %s"
               (mapconcat 'identity elisp-lint-ignored-validators ", ")))
    (let ((success t))
      (mapc (lambda (validator)
              (setq success (and (elisp-lint--run validator file) success)))
            elisp-lint-file-validators)
      (mapc (lambda (validator)
              (setq success (and (elisp-lint--run validator) success)))
            elisp-lint-buffer-validators)
      success)))

(defun elisp-lint-files-batch ()
  "Run validators on all files specified on the command line."
  (elisp-lint--amend-ignored-validators-from-command-line)
  (let ((success t))
    (while command-line-args-left
      (message "%s" (make-string 75 ?\*))
      (message "** ELISP-LINT: check %s..." (car command-line-args-left))
      (if (elisp-lint-file (car command-line-args-left))
          (message "** ELISP-LINT: %s OK" (car command-line-args-left))
        (message "** ELISP:LINT: %s FAIL" (car command-line-args-left))
        (setq success nil))
      (pop command-line-args-left))
    (kill-emacs (if success 0 1))))

(provide 'elisp-lint)

;;; elisp-lint.el ends here
