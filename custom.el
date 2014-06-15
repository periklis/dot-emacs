;;
;; Emacs configuration
;;
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq ident-tabs-mode nil)
(setq tab-always-indent 'complete)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-safe-themes (quote ("e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ecb-auto-update-methods-after-save t)
 '(ecb-force-reparse-when-semantic-idle-scheduler-off t)
 '(ecb-layout-name "left15")
 '(ecb-major-modes-show-or-hide (quote ((php-mode))))
 '(ecb-methods-menu-sorter nil)
 '(ecb-non-semantic-exclude-modes (quote (sh-mode fundamental-mode text-mode)))
 '(ecb-options-version "2.40")
 '(ecb-post-process-semantic-taglist (quote ((c++-mode ecb-group-function-tags-with-parents) (emacs-lisp-mode ecb-group-function-tags-with-parents) (c-mode ecb-filter-c-prototype-tags))))
 '(ecb-source-path (quote (("/" "/"))))
 '(flymake-no-changes-timeout 5)
 '(global-semantic-highlight-edits-mode t)
 '(global-semantic-idle-completions-mode t nil (semantic/idle))
 '(global-semantic-stickyfunc-mode t)
 '(global-visual-line-mode t)
 '(initial-frame-alist (quote ((fullscreen . fullscreen))))
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
