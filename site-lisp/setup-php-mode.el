;;; setup-php-mode.el --- PHP Mode configuration

;;; Commentary:

;;; PHP Mode configuration

;;; Code:

(require 'php-mode)

(defun setup-php-mode-ac-sources ()
  "Set the ac-sources for php-mode."
  (setq ac-sources '(ac-source-semantic ac-source-filename ac-source-yasnippet)))

(add-hook 'php-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'php-mode-hook 'electric-indent-mode)
(add-hook 'php-mode-hook 'electric-layout-mode)
(add-hook 'php-mode-hook 'electric-pair-mode)
(add-hook 'php-mode-hook 'subword-mode)
(add-hook 'php-mode-hook 'linum-mode)
(add-hook 'php-mode-hook 'my-semantic-init-hook)
(add-hook 'php-mode-hook 'yas-minor-mode)
(add-hook 'php-mode-hook 'auto-complete-mode)
(add-hook 'php-mode-hook 'c-toggle-auto-newline)
(add-hook 'php-mode-hook 'c-toggle-hungry-state)
(add-hook 'php-mode-hook 'setup-php-mode-ac-sources)

(provide 'setup-php-mode)
;;; setup-php-mode.el ends here
