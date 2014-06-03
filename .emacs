;;
;; Basic configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;;
;; Speedbar sr-mode
;;
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-max-width 50)
(setq sr-speedbar-width-x 25)
(setq sr-speedbar-width-console 25)
(setq sr-speedbar-auto-refresh t)
(require 'sr-speedbar)

;;
;; Flymake configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flymake")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flymake-cursor")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flymake-phpcs")

(setq flymake-cursor-auto-enable t)
(setq flymake-no-changes-timeout 5)
(setq flymake-max-parallel-syntax-checks 8)
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq flymake-run-in-place nil)
(setq flymake-run-in-place nil)
(setq flymake-number-of-errors-to-display nil)
(setq flymake-phpcs-command "~/.emacs.d/site-lisp/emacs-flymake-phpcs/bin/flymake_phpcs")
(setq flymake-phpcs-standard "mo4-coding-standard")
(setq flymake-phpcs-show-rule t)

(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-phpcs)

;;
;; PHP-Mode configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/php-mode")
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
(require 'php-mode)

;;
;; Emacs configuration
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(flymake-no-changes-timeout 5)
 '(global-visual-line-mode t)
 '(php-executable "/usr/local/bin/php")
 '(php-mode-speedbar-open nil)
 '(scroll-bar-mode nil)
 '(sr-speedbar-max-width 80)
 '(sr-speedbar-right-side nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

