;;; dependencies.el - project specific package dependencies

(use-package buttercup
  :ensure t)

(use-package undercover
  :ensure t)

(use-package let-alist
  :pin gnu                              ; version 1.0.6 on ELPA
  :ensure t)

(use-package package-lint
  :ensure t)

(use-package dash
  :ensure t)

;;; dependencies.el ends here
