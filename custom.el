;;
;; Emacs configuration
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bbdb-complete-name-allow-cycling t)
 '(bbdb-message-all-addresses t)
 '(bbdb-mua-auto-update-init (quote message))
 '(bbdb-mua-update-interactive-p (quote (query . create)))
 '(bbdb-north-american-phone-numbers-p nil)
 '(bbdb-offer-to-create t t)
 '(bbdb-use-pop-up nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-completion-addsuffix t)
 '(comint-input-ignoredups t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output nil)
 '(company-auto-complete (quote company-explicit-action-p))
 '(company-idle-delay 0.5)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-transformers (quote (company-sort-by-backend-importance)))
 '(compilation-read-command nil)
 '(compilation-scroll-output (quote first-error))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-ls-F-marks-symlinks t)
 '(diredp-hide-details-initially-flag nil)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average 1)
 '(display-time-mode t)
 '(ecb-auto-update-methods-after-save t)
 '(ecb-force-reparse-when-semantic-idle-scheduler-off t)
 '(ecb-layout-name "left9")
 '(ecb-major-modes-show-or-hide (quote ((php-mode js2-mode haskell-mode))))
 '(ecb-methods-menu-sorter nil)
 '(ecb-non-semantic-exclude-modes (quote (sh-mode fundamental-mode text-mode)))
 '(ecb-options-version "2.40")
 '(ecb-post-process-semantic-taglist
   (quote
    ((c++-mode ecb-group-function-tags-with-parents)
     (emacs-lisp-mode ecb-group-function-tags-with-parents)
     (c-mode ecb-filter-c-prototype-tags))))
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 45)
 '(eclimd-default-workspace "~/Projects/workspace")
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function nil)
 '(flycheck-pos-tip-timeout 10)
 '(fringe-mode (quote (4 . 0)) nil (fringe))
 '(global-auto-revert-non-file-buffers t)
 '(global-hl-line-mode t)
 '(global-linum-mode nil)
 '(global-visual-line-mode t)
 '(gnus-asynchronous t)
 '(gnus-auto-center-summary t)
 '(gnus-auto-select-next nil)
 '(gnus-auto-select-same t)
 '(gnus-buffer-configuration
   (quote
    ((group
      (vertical 1.0
                (group 1.0 point)))
     (summary
      (horizontal 1.0
                  (vertical 0.25
                            (group 1.0))
                  (vertical 1.0
                            (summary 1.0 point))))
     (article
      (cond
       (gnus-use-trees
        (quote
         (vertical 1.0
                   (summary 0.25 point)
                   (tree 0.25)
                   (article 1.0))))
       (t
        (quote
         (horizontal 1.0
                     (vertical 0.4
                               (summary 1.0 point))
                     (vertical 1.0
                               (article 1.0)))))))
     (server
      (vertical 1.0
                (server 1.0 point)))
     (browse
      (vertical 1.0
                (browse 1.0 point)))
     (message
      (vertical 1.0
                (message 1.0 point)))
     (pick
      (vertical 1.0
                (article 1.0 point)))
     (info
      (vertical 1.0
                (info 1.0 point)))
     (summary-faq
      (vertical 1.0
                (summary 0.25)
                (faq 1.0 point)))
     (only-article
      (vertical 1.0
                (article 1.0 point)))
     (edit-article
      (vertical 1.0
                (article 1.0 point)))
     (edit-form
      (vertical 1.0
                (group 0.5)
                (edit-form 1.0 point)))
     (edit-score
      (vertical 1.0
                (summary 0.25)
                (edit-score 1.0 point)))
     (edit-server
      (vertical 1.0
                (server 0.5)
                (edit-form 1.0 point)))
     (post
      (vertical 1.0
                (post 1.0 point)))
     (reply
      (vertical 1.0
                (article 0.5)
                (message 1.0 point)))
     (forward
      (vertical 1.0
                (message 1.0 point)))
     (reply-yank
      (vertical 1.0
                (message 1.0 point)))
     (mail-bounce
      (vertical 1.0
                (article 0.5)
                (message 1.0 point)))
     (pipe
      (vertical 1.0
                (summary 0.25 point)
                ("*Shell Command Output*" 1.0)))
     (bug
      (vertical 1.0
                (if gnus-bug-create-help-buffer
                    (quote
                     ("*Gnus Help Bug*" 0.5)))
                ("*Gnus Bug*" 1.0 point)))
     (score-trace
      (vertical 1.0
                (summary 0.5 point)
                ("*Score Trace*" 1.0)))
     (score-words
      (vertical 1.0
                (summary 0.5 point)
                ("*Score Words*" 1.0)))
     (split-trace
      (vertical 1.0
                (summary 0.5 point)
                ("*Split Trace*" 1.0)))
     (category
      (vertical 1.0
                (category 1.0)))
     (compose-bounce
      (vertical 1.0
                (article 0.5)
                (message 1.0 point)))
     (display-term
      (vertical 1.0
                ("*display*" 1.0)))
     (mml-preview
      (vertical 1.0
                (message 0.5)
                (mml-preview 1.0 point))))) t)
 '(gnus-decay-scores t)
 '(gnus-default-adaptive-score-alist
   (quote
    ((gnus-unread-mark)
     (gnus-ticked-mark
      (from 4))
     (gnus-dormant-mark
      (from 5))
     (gnus-del-mark
      (from -4)
      (subject -1))
     (gnus-read-mark
      (from 4)
      (subject 2))
     (gnus-expirable-mark
      (from -1)
      (subject -1))
     (gnus-killed-mark
      (from -1)
      (subject -3))
     (gnus-kill-file-mark)
     (gnus-ancient-mark)
     (gnus-low-score-mark)
     (gnus-catchup-mark
      (from -1)
      (subject -1)))))
 '(gnus-extract-address-components (quote mail-extract-address-components))
 '(gnus-gcc-mark-as-read t)
 '(gnus-group-line-format "%M%S%5y:%B%(%G%)
")
 '(gnus-select-method (quote (nnnil "")))
 '(gnus-subthread-sort-functions
   (quote
    (gnus-thread-sort-by-number gnus-thread-sort-by-date)))
 '(gnus-sum-thread-tree-leaf-with-other "+-> ")
 '(gnus-sum-thread-tree-root "")
 '(gnus-sum-thread-tree-single-indent "")
 '(gnus-sum-thread-tree-single-leaf "`-> ")
 '(gnus-sum-thread-tree-vertical "|")
 '(gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s
")
 '(gnus-summary-same-subject "")
 '(gnus-thread-hide-subtree t)
 '(gnus-thread-ignore-subject t)
 '(gnus-thread-indent-level 2)
 '(gnus-thread-sort-functions
   (quote
    (gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-score)))
 '(gnus-treat-hide-citation t)
 '(gnus-use-adaptive-scoring t)
 '(gnus-use-cache t)
 '(helm-M-x-fuzzy-match t)
 '(helm-ack-base-command "ack -H --nogroup")
 '(helm-apropos-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-command-prefix-key "C-c h")
 '(helm-echo-input-in-header-line t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-grep-default-command "grep -a -d recurse %e -n%cH -e %p %f")
 '(helm-gtags-auto-update t)
 '(helm-gtags-path-style (quote root))
 '(helm-gtags-prefix-key "g")
 '(helm-gtags-suggested-key-mapping t)
 '(helm-imenu-fuzzy-match t)
 '(helm-locate-fuzzy-match t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-net-prefer-curl t)
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(helm-scroll-amount 8)
 '(helm-semantic-fuzzy-match t)
 '(helm-split-window-in-side-p t)
 '(helm-swoop-move-to-line-cycle t)
 '(helm-swoop-split-direction (quote split-window-horizontally))
 '(helm-swoop-use-line-number-face t)
 '(helm-time-zone-home-location "Berlin")
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(jabber-account-list (quote (("periklis.tsirakidis@mayflower.de"))))
 '(jabber-alert-presence-message-function (quote periklis/jabber-alert-func))
 '(jabber-auto-reconnect t)
 '(jabber-chat-buffer-format "%n")
 '(jabber-chat-buffer-show-avatar nil)
 '(jabber-connection-ssl-program nil)
 '(jabber-debug-log-xml nil)
 '(jabber-groupchat-buffer-format "%n")
 '(jabber-mode-line-mode t)
 '(jabber-muc-autojoin
   (quote
    ("administrator@conference.mayflower.de" "devops@conference.mayflower.de" "opensource@conference.mayflower.de" "mayflower@conference.mayflower.de" "man@conference.mayflower.de" "bizdev@conference.mayflower.de" "promo@conference.mayflower.de" "ergodirekt@conference.mayflower.de")))
 '(jabber-muc-default-nicknames
   (quote
    (("administrator@conference.mayflower.de" . "periklis")
     ("devops@conference.mayflower.de" . "periklis")
     ("opensource@conference.mayflower.de" . "periklis")
     ("mayflower@conference.mayflower.de" . "periklis")
     ("man@conference.mayflower.de" . "periklis")
     ("bizdev@conference.mayflower.de" . "periklis")
     ("promo@conference.mayflower.de" . "periklis")
     ("ergodirekt@conference.mayflower.de" . "periklis"))))
 '(jabber-muc-private-buffer-format "%g-%n")
 '(jabber-nickname "periklis")
 '(jabber-roster-buffer "roster")
 '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
 '(jabber-roster-show-bindings nil)
 '(jabber-roster-show-title nil)
 '(jabber-roster-sort-functions
   (quote
    (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group)))
 '(load-prefer-newer t)
 '(magit-diff-options (quote ("-b")))
 '(magit-last-seen-setup-instructions "1.4.0")
 '(max-specpdl-size 1600)
 '(message-kill-buffer-on-exit t)
 '(multi-term-program "~/.nix-profile/bin/zsh")
 '(multi-term-scroll-show-maximum-output nil)
 '(multi-term-scroll-to-bottom-on-output "this")
 '(multi-term-switch-after-close nil)
 '(nxml-attribute-indent 4)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-child-indent 4)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files
   (quote
    ("~/org/notes.org" "~/org/work/" "~/org/calendars/")))
 '(org-agenda-include-diary t)
 '(org-archive-location "~/org/datetrees/archive.org::datetree/* Archived Tasks")
 '(org-capture-templates
   (quote
    (("a" "Account Task" entry
      (file+headline "~/org/work/accounts.org" "Open Tasks")
      "* TODO %?
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))
%a

")
     ("b" "Bookmark" entry
      (file+headline "~/org/bookmarks.org" "Unsorted Bookmarks")
      "* %(periklis/org-capture-annotation \"%a\") %?
%(periklis/org-capture-prop-template \"%a\" \"%U\")")
     ("l" "Review link" entry
      (file+headline "~/org/notes.org" "Links")
      "* TODO Review: %c %i %?
%(periklis/org-capture-prop-template \"%a\" \"%U\")")
     ("n" "Note" entry
      (file+headline "~/org/notes.org" "Tasks")
      "* TODO %? %i
%(periklis/org-capture-prop-template \"%a\" \"%U\")")
     ("r" "Read item" entry
      (file+headline "~/org/notes.org" "Reading List")
      "* TODO Read: %(periklis/org-capture-annotation \"%a\") %?
%(periklis/org-capture-prop-template \"%a\" \"%U\")")
     ("w" "Watch item" entry
      (file+headline "~/org/notes.org" "Watch List")
      "* TODO Watch: %(periklis/org-capture-annotation \"%a\") %?
%(periklis/org-capture-prop-template \"%a\" \"%U\")"))))
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/org")
 '(org-projectile:projects-file "~/org/work/projects.org")
 '(org-refile-targets
   (quote
    ((nil :level . 1)
     ("~/org/work/accounts.org" :level . 2)
     ("~/org/work/projects.org" :level . 1))))
 '(org-special-ctrl-a/e t)
 '(org-tags-column -120)
 '(package-selected-packages
   (quote
    (penrspeen perspeen zlc yaml-mode xterm-color xml-rpc whitespace-cleanup-mode which-key wgrep-helm wget w3m vagrant-tramp vagrant use-package undo-tree twig-mode tide term+ ssh-config-mode ssh srefactor smartparens smart-mode-line-powerline-theme sass-mode rtags restclient request-deferred puppet-mode phpunit phpcbf php-refactor-mode php-extras php-eldoc php-auto-yasnippets persp-projectile pdf-tools pcache paredit paradox pandoc-mode ox-pandoc orgit org-projectile oauth2 markdown-mode magit-filenotify macrostep linum-relative karma json-mode js2-refactor jenkins javadoc-lookup jasminejs-mode jabber-otr itail info+ inf-php history highlight-symbol highlight-numbers helm-swoop helm-mt helm-gtags helm-git-grep helm-fuzzier helm-flycheck helm-descbinds helm-bbdb helm-ag hardcore-mode handoff guide-key google-translate google-maps google-c-style google gitignore-mode gitconfig-mode git-timemachine ghc geben-helm-projectile flycheck-pos-tip flycheck-haskell flx-ido expand-region engine-mode emojify emacs-eclim ecb duplicate-thing dired-sort-menu+ dired-narrow dired+ dash-at-point ctags ctable concurrent company-tern company-quickhelp company-c-headers color-theme-solarized cmake-mode cmake-ide cl-generic circe bash-completion auto-compile alert ac-js2)))
 '(paradox-github-token t)
 '(php-auto-yasnippet-php-program
   (expand-file-name "elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php" user-emacs-directory) t)
 '(php-mode-speedbar-open nil)
 '(php-refactor-command "refactor")
 '(phpunit-arg "")
 '(phpunit-program "phpunit --colors --disallow-test-output")
 '(phpunit-stop-on-error t)
 '(phpunit-stop-on-failure t)
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
 '(projectile-mode-line-lighter "")
 '(restclient-log-request nil)
 '(ring-bell-function (quote ignore))
 '(rtags-autostart-diagnostics t)
 '(rtags-completions-enabled t)
 '(rtags-use-helm t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(solarized-termcolors 256)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(too-hardcore-backspace t)
 '(too-hardcore-return t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(visible-bell nil)
 '(wgrep-auto-save-buffer t)
 '(which-key-sort-order (quote which-key-key-order-alpha))
 '(which-key-use-C-h-commands t)
 '(winner-mode t)
 '(yas-prompt-functions (quote (yas-completing-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffer-directory ((t (:foreground "#657b83"))))
 '(helm-ff-directory ((t nil)))
 '(helm-selection ((t (:background "gainsboro" :underline t))))
 '(helm-source-header ((t (:background "#eee8d5" :foreground "#839496" :weight bold :height 1.3 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "gainsboro"))))
 '(hl-line ((t (:inherit highlight :background "gainsboro" :underline nil)))))
