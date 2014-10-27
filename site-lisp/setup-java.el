;;; setup-java.el --- Java Mode condiguration

;;; Commentary:

;;; Java Mode condiguration

;;; Code:

(add-hook 'java-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'java-mode-hook 'electric-indent-mode)
(add-hook 'java-mode-hook 'electric-layout-mode)
(add-hook 'java-mode-hook 'electric-pair-mode)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook 'linum-mode)
(add-hook 'java-mode-hook 'my-semantic-init-hook)
(add-hook 'java-mode-hook 'auto-complete-mode)
(add-hook 'java-mode-hook 'c-toggle-auto-newline)
(add-hook 'java-mode-hook 'c-toggle-hungry-state)

(provide 'setup-java)
;;; setup-java.el ends here
