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
;;; Change Log:
;;
;;    Initial release.
;;
;;; Code:

;; helpers

(require 'bytecomp)
(require 'package nil t)

(defmacro elisp-lint--protect (&rest body)
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error (message "%s" (error-message-string err)) nil)))

(defmacro elisp-lint--run (&rest body)
  (declare (indent 0) (debug t))
  `(setq success (and (elisp-lint--protect ,@body) success)))

;; validators

(defun elisp-lint--byte-compile (file)
  (let ((byte-compile-error-on-warn t)
        (byte-compile-warnings t))
    (byte-compile-file file)))

(defun elisp-lint--package-format ()
  (or (null (require 'package nil t))
      (package-buffer-info)))

(defun elisp-lint--indent ()
  (let ((tick (buffer-modified-tick)))
    (indent-region (point-min) (point-max))
    (or (equal tick (buffer-modified-tick))
        (error "Indentation incorrect."))))

;; linting

(defun elisp-lint-file (file)
  (let ((success t))
    (elisp-lint--run (elisp-lint--byte-compile file))
    (with-temp-buffer
      (find-file file)
      (elisp-lint--run (elisp-lint--package-format))
      (elisp-lint--run (elisp-lint--indent)))
    success))

(defun elisp-lint-files-batch ()
  (let ((success t))
    (while command-line-args-left
      (setq success (and (elisp-lint-file (car command-line-args-left)) success)
            command-line-args-left (cdr command-line-args-left)))
    (kill-emacs (if success 0 1))))

(provide 'elisp-lint)
;;; elisp-lint.el ends here
