;;; setup-elisp.el --- Emacs Lisp configuration

;;; Commentary:

;;; Emacs Lisp configuration

;;; Code:

(add-hook 'emacs-lisp-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'emacs-lisp-mode-hook 'electric-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-layout-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'subword-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)

(provide 'setup-elisp)
;;; setup-elisp.el ends here
