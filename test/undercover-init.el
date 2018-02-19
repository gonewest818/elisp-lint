;;; undercover-init.el --- setup undercover

;; `load-file` this into any other file containing tests

(when (require 'undercover nil t)
  (undercover "*.el"
              (:report-file "coverage.json")
              (:send-report nil)))

;;; undercover-init.el ends here
