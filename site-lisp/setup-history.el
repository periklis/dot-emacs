;;; setup-history.el --- History configuration

;;; Commentary:

;;; History configuration

;;; Code:

(require 'history)

;; Add history just before `find-tag' executed.
(add-to-list 'history-advised-before-functions 'find-tag-noselect t)

;; Add history just before `find-file' executed.
(add-to-list 'history-advised-before-functions 'find-file-noselect t)

(provide 'setup-history)
;;; setup-history.el ends here
