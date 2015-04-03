;;; setup-hooks.el --- Hook configuration

;;; Commentary:

;;; Emacs Hook configuration

;;; Code:

(add-hook 'emacs-lisp-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'emacs-lisp-mode-hook 'electric-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-layout-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'subword-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'after-init-hook (lambda () (load-theme 'solarized-light t)))

(provide 'setup-hooks)

;;; setup-hooks.el ends here
