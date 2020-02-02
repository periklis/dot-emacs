;;; key-bindings.el --- Global key configuration

;;; Commentary:

;;; Global key configuration

;;; Code:

;; register mac specific keys for remote emacs session over ssh
(when (and (equal system-type 'darwin)
           (not (equal window-system nil)))
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq mac-command-modifier nil)
  (setq mac-function-modifier 'super))

(when (equal window-system nil)
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq mac-command-modifier nil)
  (setq mac-function-modifier 'super))

;; compile-command
(global-set-key (kbd "C-x C-m") 'compile)

;; auto-indent newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; Eval data expression
(global-set-key "\M-:" 'data-debug-eval-expression)

(provide 'key-bindings)
;;; key-bindings.el ends here
