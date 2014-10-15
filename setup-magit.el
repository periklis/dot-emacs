;;; setup-ecb.el --- Magit configuration

;;; Commentary:

;;; Magit configuration

;;; Code:

(setq magit-diff-options '("-b"))

;; Enable this when emacs-mac uses emacs 24.4 codebase
;;(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(provide 'setup-magit)
;;; setup-magit.el ends here
