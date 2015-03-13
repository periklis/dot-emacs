;;; setup-flycheck.el --- Flycheck configuration

;;; Commentary:

;;; flycheck configuration

;;; Code:

(setq flycheck-display-errors-function nil)
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
