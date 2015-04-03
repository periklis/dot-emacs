;;
;; Emacs configuration
;;
(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(global-hl-line-mode t)
 '(global-linum-mode nil)
 '(global-semantic-highlight-edits-mode t)
 '(global-semantic-idle-completions-mode t nil (semantic/idle))
 '(global-semantic-stickyfunc-mode t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(restclient-inhibit-cookies t)
 '(restclient-log-request nil)
 '(ring-bell-function (quote ignore) t)
 '(scroll-bar-mode nil)
 '(semantic-mode t)
 '(show-trailing-whitespace nil)
 '(sr-speedbar-max-width 80)
 '(sr-speedbar-right-side nil)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(whitespace-style
   (quote
    (tabs spaces lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hackernews-link-face ((t (:foreground "cadet blue"))))
 '(helm-buffer-directory ((t (:foreground "#657b83"))))
 '(helm-ff-directory ((t nil)))
 '(helm-selection ((t (:background "gainsboro" :underline t))))
 '(helm-source-header ((t (:background "#eee8d5" :foreground "#839496" :weight bold :height 1.3 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "gainsboro"))))
 '(hl-line ((t (:inherit highlight :background "gainsboro" :underline nil)))))
