;;; init.el --- Emacs initiliazation

;;; Commentary:

;;; Emacs initialization

;;; Code:

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set path to dependencies
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; Set path for temporary directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Functions (load all files in defuns-dir)
(defvar defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Add package manager configuration
(eval-and-compile
  (package-initialize nil)
  
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (unless (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/marmalade" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory))
    (package-refresh-contents))

  ;; (unless (file-exists-p (expand-file-name "elpa/archives/melpa-stable" user-emacs-directory))
  ;;   (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (defvar use-package-verbose t)
  
  (require 'cl)
  (require 'use-package))

;; Custom variables definitions
(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(fringe-mode '(4 . 0))
 '(global-linum-mode nil)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(ring-bell-function (quote ignore) t)
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace nil)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(whitespace-style
   (quote
    (tabs spaces lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))

;; Custom face definitions
(custom-set-faces
 '(hl-line ((t (:inherit highlight :background "gainsboro" :underline nil)))))

;; Load Libraries
(use-package async      :ensure t :defer t)
(use-package bind-key   :ensure t :defer t)
(use-package dash       :defer t :load-path "site-lisp/dash" :config (eval-after-load "dash" '(dash-enable-font-lock)))
(use-package diminish   :ensure t :defer t)
(use-package f          :ensure t :defer t)
(use-package let-alist  :ensure t :defer t)
(use-package s          :ensure t :defer t)
(use-package xml-rpc    :ensure t :defer t)

;; Load packages
(use-package auto-complete
  :ensure t
  :demand t
  :diminish auto-complete-mode
  :init
  (use-package auto-complete-exuberant-ctags :ensure t :commands auto-complete)
  (use-package ac-etags                      :ensure t :commands auto-complete)
  (use-package ac-haskell-process            :ensure t :commands haskell-mode)
  (use-package ac-helm                       :ensure t :commands helm-M-x)
  (use-package ac-js2                        :ensure t :commands (js2-mode jasminejs-mode))
  :config
  (require 'auto-complete-config)
  (require 'ac-helm)
  (setq ac-auto-start t)
  (setq ac-auto-show-menu 0.2)
  (setq ac-use-menu-map t)
  (setq ac-use-quick-help nil)
  (setq ac-ignore-case t)
  (global-auto-complete-mode)

  ;; auto-complete
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous))

(use-package bash-completion
  :ensure t
  :defer t)

(use-package ctags
  :ensure t
  :demand t
  :config
  (defvar ctags-executable "/usr/local/bin/ctags")

  (defun create-tags (languages options)
    "Create tags file."
    (interactive)
    (shell-command
     (format "%s --languages=%s --options=%s -e -R ." ctags-executable languages options)))

  (defun create-project-tags (languages options-file-name)
    "Create tags for current project."
    (interactive "sLanguages: \nsOptions: ")
    (create-tags languages options-file-name)
    (message "Created language tags (%s) for current project" languages)))

(use-package color-theme-solarized
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook (lambda () (load-theme 'solarized-light t))))

(use-package ecb
  :ensure t
  :defer t
  :commands (ecb-activate ecb-deactivate)
  :config
  (custom-set-variables
   '(ecb-auto-update-methods-after-save t)
   '(ecb-force-reparse-when-semantic-idle-scheduler-off t)
   '(ecb-layout-name "left3")
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
   '(ecb-major-modes-show-or-hide '((php-mode js2-mode haskell-mode)))))

(use-package ediff
  :defer t
  :config
  (add-hook 'ediff-load-hook 'ecb-deactivate)
  (add-hook 'ediff-quit-hook 'ecb-activate)

  (setq ediff-split-window-function 'split-window-vertically)
  (setq ediff-ignore-similar-regions t))

(use-package emacs-lisp-mode
  :demand t
  :init
  (add-hook 'emacs-lisp-mode-hook '(lambda () (setq truncate-lines 0)))
  (add-hook 'emacs-lisp-mode-hook 'linum-mode)
  (add-hook 'emacs-lisp-mode-hook 'electric-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'electric-layout-mode)
  (add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
  (add-hook 'emacs-lisp-mode-hook 'subword-mode)
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)

  (auto-fill-mode 1)
  (paredit-mode 1)
  (eldoc-mode 1)

  (local-set-key (kbd "<return>") 'paredit-newline)
  (add-hook 'after-save-hook 'check-parens nil t))

(use-package expand-region
  :ensure t
  :defer t)

(use-package flx
  :ensure t
  :demand t
  :init
  (use-package flx-ido
    :ensure t
    :demand t))

(use-package flycheck
  :ensure t
  :demand t
  :config
  (custom-set-variables
   '(flycheck-php-phpcs-executable "/usr/local/bin/phpcs")
   '(flycheck-phpcs-standard "MO4")
   '(flycheck-display-errors-function nil))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package google-maps
  :ensure t
  :commands google-maps)

(use-package google-translate
  :ensure t
  :defer t
  :commands google-translate-at-point)

(use-package hackernews
  :ensure t
  :defer t
  :config
  (custom-set-faces
   '(hackernews-link-face ((t (:foreground "cadet blue"))))))

(use-package hardcore-mode
  :ensure t
  :demand t
  :diminish hardcore-mode
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package haskell-mode
  :ensure t
  :defer t
  :mode ("\\.l?hs\\'" . haskell-mode)
  :init
  (use-package flycheck-haskell :ensure t :commands haskell-mode)
  (use-package ghc              :ensure t :commands haskell-mode)
  (add-hook 'haskell-mode-hook 'linum-mode)
  :bind (("C-c C-l" . haskell-process-load-or-reload)
         ("C-`"     . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c"   . haskell-process-cabal)
         ("SPC"     . haskell-mode-contextual-space)
         ("M-."     . haskell-mode-jump-to-def-or-tag))
  :config
  (defvar cabal-lib-dir "~/.cabal/lib/")
  (add-to-list 'load-path cabal-lib-dir)

  ;; Load cabal projects
  (dolist (project (directory-files cabal-lib-dir t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project)))

  (custom-set-variables
   '(haskell-process-auto-import-loaded-modules  t)
   '(haskell-process-log                         t)
   '(haskell-process-suggest-hoogle-imports      t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-stylish-on-save                     t)
   '(haskell-tags-on-save                        t))

  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'electric-indent-mode)
  (add-hook 'haskell-mode-hook 'electric-layout-mode)
  (add-hook 'haskell-mode-hook 'electric-pair-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :init
  (use-package helm-ack        :ensure t :demand t)
  (use-package helm-descbinds  :ensure t :demand t)
  (use-package helm-flycheck   :ensure t :demand t)
  (use-package helm-hoogle     :ensure t :commands helm-hoogle)
  (use-package helm-git-grep   :ensure t :demand t)
  (use-package helm-google     :ensure t :demand t)
  (use-package helm-projectile :ensure t :demand t)
  (use-package helm-swoop      :ensure t :demand t)
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  
  (custom-set-variables
   '(helm-grep-default-command             "grep -a -d recurse %e -n%cH -e %p %f")
   '(helm-ack-base-command                 "ack -H --nogroup")
   '(helm-time-zone-home-location          "Berlin")
   '(helm-quick-update                     t)
   '(helm-split-window-in-side-p           t)
   '(helm-buffers-fuzzy-matching           t)
   '(helm-move-to-line-cycle-in-source     t)
   '(helm-ff-search-library-in-sexp        t)
   '(helm-scroll-amount                    8)
   '(helm-ff-file-name-history-use-recentf t)
   '(helm-recentf-fuzzy-match              t)
   '(helm-buffers-fuzzy-matching           t)
   '(helm-locate-fuzzy-match               t)
   '(helm-M-x-fuzzy-match                  t)
   '(helm-semantic-fuzzy-match             t)
   '(helm-imenu-fuzzy-match                t)
   '(helm-apropos-fuzzy-match              t)
   '(helm-swoop-move-to-line-cycle         t)
   '(helm-swoop-use-line-number-face       t)
   '(helm-swoop-split-direction            'split-window-horizontally))

  (custom-set-faces
   '(helm-buffer-directory ((t (:foreground "#657b83"))))
   '(helm-ff-directory ((t nil)))
   '(helm-selection ((t (:background "gainsboro" :underline t))))
   '(helm-source-header ((t (:background "#eee8d5" :foreground "#839496" :weight bold :height 1.3 :family "Sans Serif"))))
   '(helm-visible-mark ((t (:background "gainsboro")))))
  
  ;; Load helm globaly
  (helm-mode 1)
  (helm-descbinds-mode)
  (helm-autoresize-mode 1))

(use-package history
  :ensure t
  :commands (php-mode js2-mode emacs-lisp-mode haskell-mode)
  :diminish history-mode
  :config
  (add-to-list 'history-advised-before-functions 'find-tag-noselect t)
  (add-to-list 'history-advised-before-functions 'find-file-noselect t))

(use-package geben
  :commands php-mode
  :config
  (custom-set-variables
   '(geben-dbgp-default-proxy '("10.0.2.2" 9000 "EMACS" nil t)))

  (defun my-geben-release ()
    (interactive)
    (geben-stop)
    (dolist (session geben-sessions)
      (ignore-errors
        (geben-session-release session)))))

(use-package itail
  :ensure t
  :defer t)

(use-package jabber
  :ensure t
  :demand t
  :config
  (custom-set-variables
   '(jabber-auto-reconnect t)
   '(jabber-chat-buffer-format "%n-jabber")
   '(jabber-chat-buffer-show-avatar nil)
   '(jabber-connection-ssl-program nil)
   '(jabber-groupchat-buffer-format "%n-jabber")
   '(jabber-mode-line-mode t)
   '(jabber-muc-private-buffer-format "%g-%n-jabber")
   '(jabber-roster-buffer "roster")
   '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
   '(jabber-roster-sort-functions
     (quote
      (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group))))
  (add-hook 'jabber-chat-mode-hook 'goto-address))

(use-package jasminejs-mode
  :ensure t
  :commands jasminejs-mode)

(use-package java
  :commands java-mode
  :preface
  (defun java-semantic-init-hook ()
    (semantic-mode t))
  :init
  (add-hook 'java-mode-hook 'java-semantic-init-hook)
  (add-hook 'java-mode-hook 'linum-mode)
  :config
  (add-hook 'java-mode-hook '(lambda () (setq truncate-lines 0)))
  (add-hook 'java-mode-hook 'electric-indent-mode)
  (add-hook 'java-mode-hook 'electric-layout-mode)
  (add-hook 'java-mode-hook 'electric-pair-mode)
  (add-hook 'java-mode-hook 'subword-mode)
  (add-hook 'java-mode-hook 'auto-complete-mode)
  (add-hook 'java-mode-hook 'c-toggle-auto-newline)
  (add-hook 'java-mode-hook 'c-toggle-hungry-state))

(use-package jira
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :commands json-mode
  :init
  (use-package json-reformat :ensure t :defer t)
  (use-package json-snatcher :ensure t :defer t))

(use-package js2-mode
  :ensure t
  :commands (js2-mode jasminejs-mode)
  :mode (("\\.js\\'" . js2-mode)
         ("\\.spec\\'" . js2-mode))
  :init
  (use-package js2-refactor :ensure t :defer t)
  (add-hook 'js2-mode-hook 'linum-mode)
  :config
  (setq ac-js2-evaluate-calls t)

  (add-hook 'js2-mode-hook '(lambda () (setq truncate-lines 0)))
  (add-hook 'js2-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook 'electric-indent-mode)
  (add-hook 'js2-mode-hook 'electric-layout-mode)
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package karma
  :ensure t
  :commands karma-mode)

(use-package macrostep
  :ensure t
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :ensure t
  :init
  (use-package gitconfig-mode   :ensure t)
  (use-package gitignore-mode   :ensure t)
  (use-package git-timemachine  :ensure t)
  :bind (("C-c m g" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-diff-options '("-b")))

(use-package nxml-mode
  :defer t
  :commands nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.pom\\'" . nxml-mode))
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)

  (custom-set-variables
   '(nxml-child-indent                     4)
   '(nxml-attribute-indent                 4)
   '(nxml-auto-insert-xml-declaration-flag t)
   '(nxml-bind-meta-tab-to-complete-flag   t)
   '(nxml-slash-auto-complete-flag         t)
   '(nxml-sexp-element-flag                t)))

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (custom-set-variables
   '(paradox-github-token t))
  (paradox-enable))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :diminish paredit-mode
  :config
  ;;(use-package paredit-ext)
  (bind-key "C-M-l" 'paredit-recentre-on-sexp paredit-mode-map)
  (bind-key ")" 'paredit-close-round-and-newline paredit-mode-map)
  (bind-key "M-)" 'paredit-close-round paredit-mode-map)
  (bind-key "M-k" 'paredit-raise-sexp paredit-mode-map)
  (bind-key "M-I" 'paredit-splice-sexp paredit-mode-map)
  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map)
  (bind-key "C-. D" 'paredit-forward-down paredit-mode-map)
  (bind-key "C-. B" 'paredit-splice-sexp-killing-backward paredit-mode-map)
  (bind-key "C-. C" 'paredit-convolute-sexp paredit-mode-map)
  (bind-key "C-. F" 'paredit-splice-sexp-killing-forward paredit-mode-map)
  (bind-key "C-. a" 'paredit-add-to-next-list paredit-mode-map)
  (bind-key "C-. A" 'paredit-add-to-previous-list paredit-mode-map)
  (bind-key "C-. j" 'paredit-join-with-next-list paredit-mode-map)
  (bind-key "C-. J" 'paredit-join-with-previous-list paredit-mode-map))

(use-package pdf-tools
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :demand t
  :init
  (use-package perspective
    :ensure t
    :demand t
    :init
    (use-package persp-projectile
      :ensure t
      :demand t))
  :config
  (custom-set-variables
   '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
   '(projectile-tags-command "/usr/local/bin/ctags --languages=php --options=ctags.conf -e -R .")
   '(projectile-mode-line-lighter "")
   '(projectile-enable-caching t)
   '(projectile-completion-system 'helm))

  ;; Load projectile globaly
  (projectile-global-mode)
  (persp-mode)
  (helm-projectile-on))

(use-package php-mode
  :ensure t
  :preface
  (defun php-semantic-init-hook ()
    (use-package wisent-php :demand t)
    (semantic-mode t))
  :init
  (use-package inf-php             :ensure t :commands inf-php)
  (use-package phpcbf              :ensure t :commands php-mode)
  (use-package php-eldoc           :ensure t :commands php-mode)
  (use-package php-extras          :ensure t :commands php-mode)
  (use-package php-refactor-mode   :ensure t :commands php-mode)
  (use-package php-auto-yasnippets :ensure t :commands php-mode)
  (use-package phpunit             :ensure t :commands php-mode)
  :config
  (custom-set-variables
   '(php-executable "/usr/local/bin/php")
   '(php-mode-coding-style 'symfony2)
   '(php-mode-speedbar-open nil)
   '(php-refactor-command "refactor")
   '(phpcbf-executable "/usr/local/bin/phpcbf")
   '(phpcbf-standard "MO4")
   '(phpunit-arg "")
   '(phpunit-program "phpunit --colors --disallow-test-output")
   '(phpunit-stop-on-error t)
   '(phpunit-stop-on-failure t))

  (defun setup-php-mode-ac-sources ()
    "Set the ac-sources for php-mode."
    (setq ac-sources '(ac-source-semantic ac-source-filename ac-source-dictionary ac-source-yasnippet)))

  (c-add-style
   "mo4"
   '("symfony2"))

  (defun mo4/php-enable-mo4-coding-style ()
    "Makes MO4 preferable coding style on extending Symfony2."
    (interactive)
    (setq indent-tabs-mode nil
          fill-column 120
          c-indent-comments-syntactically-p t
          require-final-newline t)
    (c-set-style "mo4"))

  (defun setup-php-eldoc-mode ()
    "Setup php eldoc content."
    (php-eldoc-enable))

  (add-hook 'php-mode-hook '(lambda () (setq truncate-lines 0)))
  (add-hook 'php-mode-hook '(lambda () (linum-mode t)))
  (add-hook 'php-mode-hook 'electric-indent-mode)
  (add-hook 'php-mode-hook 'electric-layout-mode)
  (add-hook 'php-mode-hook 'electric-pair-mode)
  (add-hook 'php-mode-hook 'subword-mode)
  (add-hook 'php-mode-hook 'php-semantic-init-hook)
  (add-hook 'php-mode-hook 'c-toggle-auto-newline)
  (add-hook 'php-mode-hook 'c-toggle-hungry-state)
  (add-hook 'php-mode-hook 'auto-complete-mode)
  (add-hook 'php-mode-hook 'setup-php-mode-ac-sources)
  (add-hook 'php-mode-hook 'setup-php-eldoc-mode)
  (add-hook 'php-mode-hook 'php-refactor-mode)
  (add-hook 'php-mode-hook 'yas-minor-mode)
  (add-hook 'php-mode-hook 'history-mode)

  ;; coding styles
  (remove-hook 'php-mode-symfony2-hook 'php-enable-symfony2-coding-style t)
  (add-hook    'php-mode-symfony2-hook 'mo4/php-enable-mo4-coding-style  nil t)

  ;;phpunit
  (define-key php-mode-map (kbd "C-x t") 'phpunit-current-test)
  (define-key php-mode-map (kbd "C-x c") 'phpunit-current-class)
  (define-key php-mode-map (kbd "C-x p") 'phpunit-current-project))

(use-package puppet-mode
  :ensure t
  :defer t
  :mode ("\\.pp\\'" . puppet-mode))

(use-package puppetfile-mode
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t
  :commands restclient-mode
  :config
  (custom-set-variables
   '(restclient-inhibit-cookies t)
   '(restclient-log-request nil)))

(use-package rich-minority
  :ensure t)

(use-package sass-mode
  :ensure t
  :defer t)

(use-package semantic
  :demand t
  :config
  (use-package semantic/ia         :demand t)
  (use-package semantic/db-ebrowse :demand t)

  (custom-set-variables
   '(global-semantic-highlight-edits-mode t)
   '(global-semantic-idle-completions-mode t nil (semantic/idle))
   '(global-semantic-stickyfunc-mode t))
  
  ;; Enabe idle semenatic modes
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

  ;; Enable semanticdb modes
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

  ;; Enable semenatic highlighting/bookmarking modes
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode-hook)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

  ;; Enable semantic status modes
  (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package shell
  :ensure t
  :demand t
  :config
  (require 'ansi-color)

  (setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i"))

  (defvar my-tramp-ssh-completions '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")))

  (mapc (lambda (method)
          (tramp-set-completion-function method my-tramp-ssh-completions))
        '("fcp" "rsync" "scp" "scpc" "scpx" "sftp" "ssh"))


  ;; (add-hook 'ssh-mode-hook (lambda ()
  ;;                            (shell-dirtrack-mode nil)
  ;;                            (setq dirtrackp nil)))

  (tramp-get-completion-function "ssh")

  (eval-after-load 'tramp
    '(vagrant-tramp-enable))

  (defun colorize-compilation-buffer ()
    "Turns ascii colors in compilation buffer."
    (read-only-mode)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package sos
  :ensure t
  :defer t)

(use-package ssh
  :ensure t
  :demand t)

(use-package ssh-config-mode
  :ensure t
  :commands ssh-config-mode)

(use-package sudo-ext
  :ensure t
  :defer t)

(use-package sx
  :ensure t
  :commands (sx-ask sx-inbox sx-search))

(use-package twig-mode
  :ensure t
  :commands twig-mode)

(use-package vagrant
  :ensure t
  :commands (vagrant-up vagrant-resume vagrant-suspend)
  :init
  (use-package vagrant-tramp
    :ensure t
    :defer t))

(use-package whitespace-cleanup-mode
  :ensure t
  :demand t
  :diminish whitespace-cleanup-mode
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package wget
  :ensure t
  :defer t
  :commands wget)

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :diminish  yas-minor-mode)

;; Add global key bindings
(use-package key-bindings
  :demand t)

;; Load local machine specific stuff
(defvar local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file)
  (load local-file))
;;; init.el ends here
