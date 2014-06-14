;;
;; php-mode configuration
;;
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
(add-hook 'php-mode-hook (lambda () (linum-mode 1)))
(add-hook 'php-mode-hook (lambda () (setq truncate-lines 0)))

(provide 'setup-php-mode)
