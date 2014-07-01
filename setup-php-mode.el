;;
;; php-mode configuration
;;
(require 'php-mode)

(add-hook 'php-mode-hook '(lambda () (subword-mode 1)))
(add-hook 'php-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'php-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'php-mode-hook '(lambda () (my-semantic-init-hook)))
(add-hook 'php-mode-hook '(lambda () (yas-minor-mode)))

(provide 'setup-php-mode)
