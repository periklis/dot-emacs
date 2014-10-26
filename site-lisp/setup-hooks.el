;;; setup-hooks.el --- Hook configuration

;;; Commentary:

;;; Emacs Hook configuration

;;; Code:

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'focus-out-hook 'save-buffer)

(provide 'setup-hooks)

;;; setup-hooks.el ends here
