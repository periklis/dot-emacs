;;; setup-php-mode.el --- PHP Mode configuration

;;; Commentary:

;;; PHP Mode configuration

;;; Code:

(require 'php-mode)
(require 'php-refactor-mode)
(require 'php-extras)

(defun setup-php-mode-ac-sources ()
  "Set the ac-sources for php-mode."
  (setq ac-sources '(ac-source-semantic ac-source-filename ac-source-dictionary ac-source-yasnippet)))

(custom-set-variables
 '(php-executable "/usr/local/bin/php")
 '(php-mode-coding-style (quote symfony2))
 '(php-mode-speedbar-open nil)
 '(php-refactor-command "refactor"))

(add-hook 'php-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'php-mode-hook 'electric-indent-mode)
(add-hook 'php-mode-hook 'electric-layout-mode)
(add-hook 'php-mode-hook 'electric-pair-mode)
(add-hook 'php-mode-hook 'subword-mode)
(add-hook 'php-mode-hook 'linum-mode)
(add-hook 'php-mode-hook 'my-semantic-init-hook)
(add-hook 'php-mode-hook 'c-toggle-auto-newline)
(add-hook 'php-mode-hook 'c-toggle-hungry-state)
(add-hook 'php-mode-hook 'auto-complete-mode)
(add-hook 'php-mode-hook 'setup-php-mode-ac-sources)
(add-hook 'php-mode-hook 'php-refactor-mode)
(add-hook 'php-mode-hook 'yas-minor-mode)
(add-hook 'php-mode-hook 'history-mode)

(provide 'setup-php-mode)
;;; setup-php-mode.el ends here
