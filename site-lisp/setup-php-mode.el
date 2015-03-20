;;; setup-php-mode.el --- PHP Mode configuration

;;; Commentary:

;;; PHP Mode configuration

;;; Code:

(require 'php-mode)
(require 'phpcbf)
(require 'php-refactor-mode)
(require 'php-extras)
(require 'php-eldoc)

(custom-set-variables
 '(php-executable "/usr/local/bin/php")
 '(php-mode-coding-style (quote symfony2))
 '(php-mode-speedbar-open nil)
 '(php-refactor-command "refactor")
 '(phpcbf-executable "/usr/local/bin/phpcbf")
 '(phpcbf-standard "MO4")
 '(flycheck-php-phpcs-executable "/usr/local/bin/phpcs")
 '(flycheck-phpcs-standard "MO4")
 '(php-eldoc-probe-executable concat(php-executable " /usr/local/bin/probe.php")))

(defun setup-php-mode-ac-sources ()
  "Set the ac-sources for php-mode."
  (setq ac-sources '(ac-source-semantic ac-source-filename ac-source-dictionary ac-source-yasnippet)))

(defun setup-php-eldoc-mode ()
  "Setup php eldoc content."
  (php-eldoc-enable)
  (php-eldoc-probe-load 'php-eldoc-probe-executable))

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
(add-hook 'php-mode-hook 'setup-php-eldoc-mode)
(add-hook 'php-mode-hook 'php-refactor-mode)
(add-hook 'php-mode-hook 'yas-minor-mode)
(add-hook 'php-mode-hook 'history-mode)

(provide 'setup-php-mode)
;;; setup-php-mode.el ends here
