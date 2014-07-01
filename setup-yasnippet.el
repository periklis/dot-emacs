;;
;; yasnippet configuration
;;
(require 'yasnippet)
(require 'php-auto-yasnippets)

(setq php-auto-yasnippet-php-program
      (expand-file-name
       "~/.emacs.d/elpa/php-auto-yasnippets-20140515.2052/Create-PHP-YASnippet.php"))

(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(yas-reload-all)

(provide 'setup-yasnippet)
