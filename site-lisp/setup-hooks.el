;;; setup-hooks.el --- Hook configuration

;;; Commentary:

;;; Emacs Hook configuration

;;; Code:

(add-hook 'before-save-hook 'whitespace-cleanup)

;; Load default theme
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-light t)))

(provide 'setup-hooks)

;;; setup-hooks.el ends here
