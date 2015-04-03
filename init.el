;;; init.el --- Emacs initiliazation

;;; Commentary:

;;; Emacs initialization

;;; Code:

(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

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
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
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
  (defvar use-package-verbose t)  
  ;; (defvar use-package-expand-minimally t)
  (require 'setup-package)
  (require 'cl)
  (require 'use-package))

;; Load Libraries
(use-package dash            :ensure t :defer 1 :load-path "site-lisp/dash" :config (eval-after-load "dash" '(dash-enable-font-lock)))
(use-package bind-key        :ensure t :defer 1)
(use-package diminish        :ensure t :defer 1)
(use-package itail           :ensure t :defer t)
(use-package expand-region   :ensure t :defer t)
(use-package bash-completion :ensure t :defer t)
(use-package xml-rpc         :ensure t :defer t)

;; Load tools
(use-package pdf-tools  :ensure t :defer t)
(use-package restclient :ensure t :defer t)
(use-package wget       :ensure t :defer t)

;; Load web utils
(use-package google-translate :ensure t :defer t)
(use-package hackernews       :ensure t :defer t)
(use-package sos              :ensure t :defer t)

;; Load packages
(use-package color-theme-solarized
  :ensure t
  :defer t)

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package auto-complete
  :ensure t
  :defer t
  :init
  (use-package auto-complete-exuberant-ctags
    :ensure t)
  (use-package ac-etags
    :ensure t)
  (use-package ac-haskell-process
    :ensure t)
  (use-package ac-helm
    :ensure t)
  (use-package ac-js2
    :ensure t)
  :config
  (require 'auto-complete-config)
  (require 'ac-helm)
  (setq ac-auto-start t)
  (setq ac-auto-show-menu 0.2)
  (setq ac-use-menu-map t)
  (setq ac-use-quick-help nil)
  (setq ac-ignore-case t)
  (global-auto-complete-mode))

(use-package ecb
  :ensure t
  :defer t
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
   '(ecb-source-path (quote (("/" "/")))))
  (setq ecb-tip-of-the-day nil)
  (setq ecb-windows-width 45)
  (setq ecb-major-modes-show-or-hide '((php-mode js2-mode haskell-mode))))

(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package history
  :ensure t)

(use-package jabber
  :ensure t
  :defer 1
  :config
  (custom-set-variables
   '(jabber-auto-reconnect t)
   '(jabber-chat-buffer-format "%n-jabber")
   '(jabber-chat-buffer-show-avatar nil)
   '(jabber-connection-ssl-program nil)
   '(jabber-groupchat-buffer-format "%n-jabber")
   '(jabber-mode-line-mode t)
   '(jabber-muc-private-buffer-format "jabber-%g-%n")
   '(jabber-roster-buffer "jabber-roster")
   '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
   '(jabber-roster-sort-functions
     (quote
      (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group)))))

(use-package jira
  :ensure t
  :defer t)

(use-package js2-mode
  :ensure t
  :defer t
  :init
  (use-package js2-refactor
    :ensure t
    :defer t))

(use-package json-mode
  :ensure t
  :defer t)

(use-package flx
  :ensure t
  :defer t
  :init
  (use-package flx-ido
    :ensure t
    :defer t))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-php-phpcs-executable "/usr/local/bin/phpcs")
  (setq flycheck-phpcs-standard "MO4")
  (setq flycheck-display-errors-function nil)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package projectile
  :ensure t
  :init
  (use-package perspective
    :ensure t
    :init
    (use-package persp-projectile
      :ensure t))
  :config
  (custom-set-variables
   '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))))

(use-package helm
  :ensure t
  :defer 1
  :init
  (use-package helm-ack        :ensure t)
  (use-package helm-descbinds  :ensure t)
  (use-package helm-flycheck   :ensure t)
  (use-package helm-hoogle     :ensure t)
  (use-package helm-git-grep   :ensure t)
  (use-package helm-google     :ensure t)
  (use-package helm-projectile :ensure t)
  (use-package helm-swoop      :ensure t)
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
  
  ;; Load helm globaly
  (helm-mode 1)
  (helm-descbinds-mode)
  (helm-autoresize-mode 1))

(use-package haskell-mode
  :ensure t
  :defer t
  :mode ("\\.l?hs\\'" . haskell-mode)
  :init
  (use-package flycheck-haskell :ensure t)
  (use-package ghc              :ensure t)
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
  (setq cabal-lib-dir "~/.cabal/lib/")
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
  (add-hook 'haskell-mode-hook 'linum-mode)
  (add-hook 'haskell-mode-hook 'electric-indent-mode)
  (add-hook 'haskell-mode-hook 'electric-layout-mode)
  (add-hook 'haskell-mode-hook 'electric-pair-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package magit
  :ensure t
  :init
  (use-package magit-filenotify :ensure t)
  (use-package gitconfig-mode   :ensure t)
  (use-package gitignore-mode   :ensure t)
  (use-package git-timemachine  :ensure t)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package php-mode
  :ensure t
  :init
  (use-package inf-php             :ensure t)
  (use-package phpcbf              :ensure t)
  (use-package php-eldoc           :ensure t)
  (use-package php-extras          :ensure t)
  (use-package php-refactor-mode   :ensure t)
  (use-package php-auto-yasnippets :ensure t)
  (use-package phpunit             :ensure t)
  :config
  (custom-set-variables
   '(php-executable "/usr/local/bin/php")
   '(php-mode-coding-style (quote symfony2))
   '(php-mode-speedbar-open nil)
   '(php-refactor-command "refactor")
   '(phpcbf-executable "/usr/local/bin/phpcbf")
   '(phpcbf-standard "MO4")
   '(phpunit-arg "")
   '(phpunit-program "phpunit --colors --disallow-test-output")
   '(phpunit-stop-on-error t)
   '(phpunit-stop-on-failure t)))

(use-package puppet-mode
  :ensure t
  :defer t)

(use-package puppetfile-mode
  :ensure t
  :defer t)

(use-package sass-mode
  :ensure t
  :defer t)

(use-package ssh
  :ensure t
  :defer t)

(use-package ssh-config-mode
  :ensure t
  :defer t)

(use-package sudo-ext
  :ensure t
  :defer t)

(use-package twig-mode
  :ensure t
  :defer t)

(use-package vagrant
  :ensure t
  :defer t
  :init
  (use-package vagrant-tramp
    :ensure t
    :defer t))

(use-package whitespace-cleanup-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :defer t)

;; Programming environment configs
;; (require 'setup-auto-complete)
;; (require 'setup-ctags)
;; (require 'setup-flycheck)
;; (require 'setup-semantic)
;; (require 'setup-ecb)
;; (require 'setup-geben)

;; Programming languages configs
;; (require 'setup-elisp)
;; (require 'setup-php-mode)
;; (require 'setup-haskell)
;; (require 'setup-java)
;; (require 'setup-js2-mode)
;; (require 'setup-xml)

;; Navigation/Project management configs
(require 'setup-projectile)
;; (require 'setup-ediff)
(require 'setup-history)

;; External tools
(require 'setup-shell)
;; (require 'setup-magit)
;; (require 'setup-jabber)

;; Add global key bindings
(require 'key-bindings)

;; Add emacs hooks
(require 'setup-hooks)

;;; init.el ends here
