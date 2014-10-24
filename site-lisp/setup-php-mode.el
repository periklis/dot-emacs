;;; setup-php-mode.el --- PHP Mode configuration

;;; Commentary:

;;; PHP Mode configuration

;;; Code:

(require 'php-mode)

(defun setup-php-mode-ac-sources ()
  "Set the ac-sources for php-mode."
  (setq ac-sources '(ac-source-semantic ac-source-filename ac-source-yasnippet)))

(add-hook 'php-mode-hook '(lambda () (subword-mode 1)))
(add-hook 'php-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'php-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'php-mode-hook '(lambda () (my-semantic-init-hook)))
(add-hook 'php-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'php-mode-hook '(lambda () (auto-complete-mode)))
(add-hook 'php-mode-hook 'setup-php-mode-ac-sources)

(provide 'setup-php-mode)
;;; setup-php-mode.el ends here
