;;; setup-yasnippet.el --- Yasnippet configuration

;;; Commentary:

;;; Yasnippet configuration

;;; Code:

(require 'yasnippet)
(require 'php-auto-yasnippets)

(setq php-auto-yasnippet-php-program
      (expand-file-name
       "elpa/php-auto-yasnippets-20140515.2052/Create-PHP-YASnippet.php"
       user-emacs-directory))

(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(yas-reload-all)

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here
