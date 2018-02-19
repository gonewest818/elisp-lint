;;; test-elisp-lint.el --- unit tests

(load-file "test/undercover-init.el")
(require 'elisp-lint)

(describe "sample test"
  (it "does nothing"
    (expect 1 :to-equal 1)))

;;; test-elisp-lint.el ends here
