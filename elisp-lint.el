;;; elisp-lint.el --- basic linting for Emacs Lisp
;;
;; Copyright (C) 2013 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: lisp
;; URL: http://github.com/nschum/elisp-lint/
;; Compatibility: GNU Emacs 23.x, GNU Emacs 24.x
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
;; This is a tool for finding certain problems in Emacs Lisp files. Use it on
;; the command line like this:
;;
;; emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el
;;
;; You can disable individual checks, by passing flags on the command line:
;;
;; emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch --no-indent *.el
;;
;; Alternatively, you can disable checks using file variables or the following
;; .dir.locals file:
;;
;; ((nil . ((elisp-lint-ignored-validators . ("fill-column")))))
;;
;; For a full list of validators, see `elisp-lint-file-validators' and
;; `elisp-lint-buffer-validators'.
;;
;;; Change Log:
;;
;;    Initial release.
;;
;;; Code:

;; helpers

(require 'bytecomp)
(require 'package nil t)

(declare-function package-buffer-info "package" t)

(defconst elisp-lint-file-validators '("byte-compile"))
(defconst elisp-lint-buffer-validators
  '("package-format" "indent" "fill-column" "trailing-whitespace"))

(defvar elisp-lint-ignored-validators nil
  "List of validators that should not be run.")
(put 'elisp-lint-ignored-validators 'safe-local-variable 'listp)

(defmacro elisp-lint--protect (&rest body)
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error (message "%s" (error-message-string err)) nil)))

(defmacro elisp-lint--run (name &rest args)
  `(or (member ,name elisp-lint-ignored-validators)
       (elisp-lint--protect (funcall (intern (concat "elisp-lint--" ,name))
                                     ,@args))))

(defun elisp-lint--amend-ignored-validators-from-command-line ()
  (while (string-match "^--no-\\([a-z-]*\\)" (car command-line-args-left))
    (add-to-list 'elisp-lint-ignored-validators
                 (match-string 1 (pop command-line-args-left)))))

;; validators

(defun elisp-lint--byte-compile (file)
  "Byte-compiles the file with all warnings enabled."
  (let ((byte-compile-error-on-warn t)
        (byte-compile-warnings t))
    (byte-compile-file file)))

(defun elisp-lint--package-format ()
  "Calls `package-buffer-info' to validate some file metadata."
  (or (null (require 'package nil t))
      (package-buffer-info)))

(defun elisp-lint--indent ()
  "Verifies that each line is indented according to `emacs-lisp-mode'."
  (let ((tick (buffer-modified-tick)))
    (indent-region (point-min) (point-max))
    (or (equal tick (buffer-modified-tick))
        (error "Indentation incorrect."))))

(defun elisp-lint--fill-column ()
  "Verifies that no line exceeds the number of columns in fill-column.
Use a file variable or a .dir.locals file to override the value."
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
                 fill-column
                 (mapconcat 'number-to-string (sort too-long-lines '<)
                            ", "))))))

(defun elisp-lint--trailing-whitespace ()
  "Verifies that no line contains trailing whitespace."
  (save-excursion
    (let ((lines nil))
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]+$" nil t)
        (push (count-lines (point-min) (point)) lines))
      (or (null lines)
          (error "Line numbers with trailing whitespace: %s"
                 (mapconcat 'number-to-string (sort lines '<) ", "))))))

;; linting

(defun elisp-lint-file (file)
  (with-temp-buffer
    (find-file file)
    (when elisp-lint-ignored-validators
      (message "Ignoring validators: %s"
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
  (elisp-lint--amend-ignored-validators-from-command-line)
  (let ((success t))
    (while command-line-args-left
      (message "%s..." (car command-line-args-left))
      (if (elisp-lint-file (car command-line-args-left))
          (message "%s...OK" (car command-line-args-left))
        (message "%s...FAIL" (car command-line-args-left))
        (setq success nil))
      (pop command-line-args-left))
    (kill-emacs (if success 0 1))))

(provide 'elisp-lint)
;;; elisp-lint.el ends here
