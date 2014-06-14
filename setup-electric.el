;;
;; Electric configuration
;;
(setq major-modes-list '(emacs-lisp-mode lisp-mode php-mode javascript-mode))
(setq electric-ident-modes-list 'major-modes-list)
(setq electric-pair-modes-list 'major-modes-list)
(setq electric-layout-modes-list 'major-modes-list)

(provide 'setup-electric)
