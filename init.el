;;; init.el --- Emacs initialization

;;; Commentary:

;;; Emacs initialization

;;; Code:

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Set path to dependencies
(defvar contrib-lisp-dir (expand-file-name "contrib" user-emacs-directory))
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defvar styles-lisp-dir (expand-file-name "styles" user-emacs-directory))

;; Set path for temporary directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path contrib-lisp-dir)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path styles-lisp-dir)

;; Set up load path on nix-environments
(defvar nix-env-p nil)
(defvar nix-site-lisp "~/.nix-profile/share/emacs/site-lisp/")
(when (file-accessible-directory-p nix-site-lisp)
  (setq nix-env-p t)
  (add-to-list 'load-path nix-site-lisp)
  (add-to-list 'load-path (expand-file-name "rtags" nix-site-lisp)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

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
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Add package manager configuration
(eval-and-compile
  (package-initialize nil)

  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

  (unless (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/marmalade" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/melpa-stable" user-emacs-directory))
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (custom-set-variables
   '(use-package-verbose nil)
   '(use-package-enable-imenu-support t))

  (require 'cl)
  (require 'use-package))

;; Custom variables definitions
(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-read-command nil)
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-input-ignoredups t)
 '(comint-completion-addsuffix t)
 '(display-battery-mode t)
 '(display-time-default-load-average 1)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(fringe-mode '(4 . 0))
 '(global-linum-mode nil)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(load-prefer-newer t)
 '(max-specpdl-size 1600)
 '(message-kill-buffer-on-exit t)
 '(network-security-level 'paranoid)
 '(ring-bell-function (quote ignore) t)
 '(scroll-bar-mode nil)
 '(scroll-margin 0)
 '(scroll-conservatively 100000)
 '(scroll-preserve-screen-position t)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(system-uses-terminfo nil)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(winner-mode t))

;; Custom face definitions
(custom-set-faces
 '(hl-line ((t (:inherit highlight :background "gainsboro" :underline nil))))
 '(perspeen-selected-face ((t (:weight bold :foreground "Black" :background nil))))
 '(perspeen-tab--powerline-inactive1 ((t (:inherit mode-line))))
 '(perspeen-tab--header-line-active ((t (:weight bold :foreground "#58675" :background "#eee8d5"))))
 '(perspeen-tab--header-line-inactive ((t (:foreground "#839496" :background "#eee8d5")))))

;; Custom general hooks
(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'prog-mode-hook #'goto-address-mode)
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'after-init-hook #'server-mode)

;; Load Libraries
(use-package async           :ensure t :defer t)
(use-package bind-key        :ensure t :defer t)
(use-package dash            :ensure t :defer t :config (eval-after-load "dash" '(dash-enable-font-lock)))
(use-package diminish        :ensure t :defer t)
(use-package duplicate-thing :ensure t :bind ("C-c C-d" . duplicate-thing))
(use-package f               :ensure t :defer t)
(use-package info+           :ensure t :commands (info))
(use-package let-alist       :ensure t :defer t)
(use-package s               :ensure t :defer t)
(use-package xml-rpc         :ensure t :defer t)

;; Load packages
(use-package alert
  :commands (alert)
  :init
  (custom-set-variables
   '(alert-default-style 'notifier)
   '(alert-severity-colors
     '((urgent   . "red")
       (high     . "orange")
       (moderate . "yellow")
       (normal   . "grey85")
       (low      . "blue")
       (trivial . "purple")))))

(use-package auto-compile
  :ensure t
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package bbdb
  :ensure t
  :demand t
  :config
  (use-package helm-bbdb :ensure t)
  (bbdb-initialize 'gnus 'message)
  (bbdb-insinuate-gnus)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (custom-set-variables
   '(bbdb-mua-update-interactive-p '(query . create))
   '(bbdb-mua-auto-update-init 'message)
   '(bbdb-offer-to-create t)
   '(bbdb-message-all-addresses t)
   '(bbdb-north-american-phone-numbers-p nil)
   '(bbdb-complete-name-allow-cycling t)
   '(bbdb-use-pop-up nil)))

(use-package calendar
  :config
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.tpp\\'" . c++-mode))
  :config
  (use-package eassist
    :bind (:map c-mode-base-map
                ("M-o" . eassist-switch-h-cpp)))
  (use-package google-c-style :ensure t :defer t)

  (use-package cmake-ide
    :ensure t
    :config
    (use-package rtags
      :if nix-env-p
      :config
      (use-package rtags-helm)
      (custom-set-variables
       '(rtags-autostart-diagnostics t)
       '(rtags-completions-enabled t)
       '(rtags-use-helm t))

      (rtags-enable-standard-keybindings)
      (rtags-diagnostics)

      (use-package flycheck-rtags)
      (use-package company-rtags)
      (defun periklis/company-rtags()
        "Push company-rtags to company-backends."
        (set (make-local-variable 'company-backends) '(company-rtags)))

      (add-hook 'c-mode-common-hook #'periklis/company-rtags)
      (add-hook 'c++-mode-common-hook #'periklis/company-rtags))
    (cmake-ide-setup))

  (add-hook 'c-mode-common-hook #'flyspell-prog-mode)
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  (add-hook 'c-mode-common-hook #'google-make-newline-indent))

(use-package ctags
  :ensure t
  :commands (create-tags create-project-tags)
  :config
  (defvar ctags-executable "~/.nix-profile/bin/ctags")

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

(use-package cmake-mode
  :ensure t
  :mode (("\\.cmake\\'" . cmake-mode)
         ("\\CMakeLists.txt\\'" . cmake-mode))
  :config
  (defun periklis/cmake-mode-company-setup ()
    "Setup company for cmake-mode."
    (set (make-local-variable 'company-backends) '(company-cmake)))

  (add-hook 'cmake-mode-hook #'periklis/cmake-mode-company-setup)
  (add-hook 'cmake-mode-hook #'flyspell-prog-mode))

(use-package color-theme-solarized
  :ensure t
  :defer t
  :init
  (custom-set-variables
   '(solarized-termcolors 256))

  (defun periklis/load-solarized-theme ()
    "Load solarized theme."
    (load-theme 'solarized t)
    (set-frame-parameter nil 'background-mode 'light)
    (set-terminal-parameter nil 'background-mode 'light)
    (enable-theme 'solarized))

  (add-hook 'after-init-hook #'periklis/load-solarized-theme))

(use-package company
  :ensure t
  :demand t
  :diminish company-mode
  :config
  (use-package company-c-headers :ensure t :defer t)
  (use-package company-quickhelp
    :ensure t
    :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
    :config (setq company-quickhelp-delay 2))
  (custom-set-variables
   '(company-idle-delay 0.5)
   '(company-auto-complete 'company-explicit-action-p)
   '(company-tooltip-align-annotations t)
   '(company-transformers '(company-sort-by-backend-importance))
   '(company-show-numbers t))

  (add-hook 'after-init-hook 'global-company-mode))

(use-package dired
  :commands (dired dired-jump)
  :config
  (custom-set-variables
   '(dired-dwim-target t)
   '(dired-ls-F-marks-symlinks t)
   '(diredp-hide-details-initially-flag nil)
   '(delete-by-moving-to-trash t)
   '(global-auto-revert-non-file-buffers t))
  (use-package dired-sort-menu
    :ensure t
    :config
    (use-package dired-sort-menu+ :ensure t))
  (add-hook 'dired-load-hook #'dired-sort-menu)
  (use-package dired-x
    :init
    :config
    (setq-default dired-omit-files-p t)
    (add-to-list 'dired-omit-extensions ".DS_Store"))
  (use-package dired+
    :ensure t
    :config
    (add-hook 'dired-before-readin-hook
              'diredp-breadcrumbs-in-header-line-mode))
  (use-package dired-aux
    :init
    (use-package dired-async
      :ensure async
      :config
      (dired-async-mode 1)))
  (put 'dired-find-alternate-file 'disabled nil)
  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

(use-package ecb
  :ensure t
  :commands (ecb-activate ecb-deactivate)
  :config
  (custom-set-variables
   '(ecb-auto-update-methods-after-save t)
   '(ecb-force-reparse-when-semantic-idle-scheduler-off t)
   '(ecb-layout-name "left9")
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

  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-vertically)
  (setq ediff-ignore-similar-regions t))

(use-package emacs-eclim
  :ensure t
  :commands (global-eclim-mode))

(use-package emacs-lisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'emacs-lisp-mode-hook #'electric-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'electric-layout-mode)
  (add-hook 'emacs-lisp-mode-hook #'electric-pair-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
  (add-hook 'emacs-lisp-mode-hook #'helm-gtags-mode)
  (add-hook 'emacs-lisp-mode-hook #'history-mode)
  (add-hook 'emacs-lisp-mode-hook #'subword-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
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
  (use-package flycheck-pos-tip
    :ensure t
    :init
    (flycheck-pos-tip-mode)
    (custom-set-variables
     '(flycheck-pos-tip-timeout 10)
     '(flycheck-display-errors-delay 0.5)))
  (custom-set-variables
   '(flycheck-display-errors-function nil)
   '(flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package gnus
  :init
  (require 'nnir)
  (require 'gnus-async)
  (load "gnus-notify+.el")
  (custom-set-variables
   '(gnus-asynchronous t)
   '(gnus-select-method '(nnnil ""))

   '(gnus-gcc-mark-as-read t)
   '(gnus-use-cache t)

   '(gnus-auto-select-next nil)
   '(gnus-auto-select-same t)
   '(gnus-auto-center-summary t)
   '(gnus-thread-hide-subtree t)
   '(gnus-thread-ignore-subject t)
   '(gnus-thread-indent-level 2)
   '(gnus-treat-hide-citation t)
   '(gnus-group-line-format "%M%S%5y:%B%(%G%)\n")
   '(gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")
   '(gnus-summary-same-subject "")
   '(gnus-sum-thread-tree-root "")
   '(gnus-sum-thread-tree-single-indent "")
   '(gnus-sum-thread-tree-leaf-with-other "+-> ")
   '(gnus-sum-thread-tree-vertical "|")
   '(gnus-sum-thread-tree-single-leaf "`-> ")
   '(gnus-extract-address-components 'mail-extract-address-components)
   '(gnus-use-adaptive-scoring t)
   '(gnus-decay-scores t)
   '(gnus-default-adaptive-score-alist
     '((gnus-unread-mark)
       (gnus-ticked-mark (from 4))
       (gnus-dormant-mark (from 5))
       (gnus-del-mark (from -4) (subject -1))
       (gnus-read-mark (from 4) (subject 2))
       (gnus-expirable-mark (from -1) (subject -1))
       (gnus-killed-mark (from -1) (subject -3))
       (gnus-kill-file-mark)
       (gnus-ancient-mark)
       (gnus-low-score-mark)
       (gnus-catchup-mark (from -1) (subject -1))))

   '(gnus-thread-sort-functions
     '(gnus-thread-sort-by-most-recent-date
       gnus-thread-sort-by-score))

   '(gnus-subthread-sort-functions
     '(gnus-thread-sort-by-number
       gnus-thread-sort-by-date)))

  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (add-hook 'gnus-message-setup-hook #'flyspell-mode)
  (add-hook 'message-mode-hook 'turn-on-orgstruct)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)

  (custom-set-variables
   '(gnus-buffer-configuration
     '((group
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
          '(vertical 1.0
                     (summary 0.25 point)
                     (tree 0.25)
                     (article 1.0)))
         (t
          '(horizontal 1.0
                       (vertical 0.4
                                 (summary 1.0 point))
                       (vertical 1.0
                                 (article 1.0))))))
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
                      '("*Gnus Help Bug*" 0.5))
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
                  (mml-preview 1.0 point)))))))

(use-package google-maps
  :ensure t
  :commands google-maps)

(use-package google-translate
  :ensure t
  :defer t
  :commands google-translate-at-point)

(use-package guess-language
  :ensure t
  :defer t
  :init
  (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (custom-set-variables
   '(guess-language-langcodes '((en . ("en_US" "English"))
                                (de . ("de_DE" "German"))))
   '(guess-language-languages '(en de))
   '(guess-language-min-paragraph-length 45))
  :diminish guess-language-mode)

(use-package hardcore-mode
  :ensure t
  :demand t
  :diminish hardcore-mode
  :init
  (custom-set-variables
   '(too-hardcore-backspace t)
   '(too-hardcore-return t))
  :config
  (global-hardcore-mode))

(use-package haskell-mode
  :disabled
  :ensure t
  :commands haskell-mode
  :mode ("\\.l?hs\\'" . haskell-mode)
  :init
  (use-package flycheck-haskell :ensure t :commands haskell-mode :disabled)
  (use-package ghc              :ensure t :commands haskell-mode :disabled)
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

  (add-hook 'haskell-mode-hook #'(lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'electric-indent-mode)
  (add-hook 'haskell-mode-hook #'electric-layout-mode)
  (add-hook 'haskell-mode-hook #'electric-pair-mode)
  (add-hook 'haskell-mode-hook #'history-mode)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :init
  (use-package helm-config     :demand t)
  (use-package helm-fuzzier    :ensure t :demand t :init (helm-fuzzier-mode))
  (use-package helm-projectile :ensure t :demand t)
  (use-package helm-ag         :ensure t :commands helm-ag)
  (use-package helm-descbinds  :ensure t :bind ("C-c h b" . helm-descbinds))
  (use-package helm-flycheck   :ensure t :bind ("C-c f e" . helm-flycheck))
  (use-package helm-git-grep   :ensure t :commands helm-git-grep)
  (use-package helm-sys        :commands helm-top)
  (use-package helm-swoop
    :ensure t
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-swoop-back-to-last-point)
           ("C-c M-i" . helm-multi-swoop)
           ("C-x M-i" . helm-multi-swoop-all)))
  (use-package helm-gtags
    :ensure t
    :commands helm-gtags-mode
    :init
    (custom-set-variables
     '(helm-gtags-prefix-key "\C-cg")
     '(helm-gtags-suggested-key-mapping t)))
  :config
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (custom-set-variables
   '(helm-grep-default-command             "grep -a -d recurse %e -n%cH -e %p %f")
   '(helm-ack-base-command                 "ack -H --nogroup")
   '(helm-time-zone-home-location          "Berlin")
   '(helm-echo-input-in-header-line        t)
   '(helm-net-prefer-curl                  t)
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
   '(helm-swoop-split-direction            'split-window-horizontally)
   '(helm-gtags-path-style                 'root)
   '(helm-gtags-auto-update                t))

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

(use-package highlight-numbers
  :ensure t
  :diminish highlight-numbers-mode
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol
  :ensure t
  :demand t
  :diminish highlight-symbol-mode
  :config
  (highlight-symbol-nav-mode)

  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))

  (setq highlight-symbol-idle-delay 0.2
        highlight-symbol-on-navigation-p t)

  (global-set-key (kbd "M-n") 'highlight-symbol-next)
  (global-set-key (kbd "M-p") 'highlight-symbol-prev))

(use-package history
  :ensure t
  :defer t
  :diminish history-mode
  :config
  (add-to-list 'history-advised-before-functions 'find-tag-noselect t)
  (add-to-list 'history-advised-before-functions 'find-file-noselect t))

(use-package hippie-exp
  :config
  (custom-set-variables
   '(hippie-expand-try-functions-list
     '(try-expand-dabbrev
       try-expand-dabbrev-all-buffers
       try-expand-dabbrev-from-kill
       try-complete-file-name-partially
       try-complete-file-name
       try-expand-all-abbrevs
       try-expand-list
       try-expand-line
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol))))

(use-package geben
  :ensure t
  :commands (geben)
  :config
  (use-package geben-helm-projectile :ensure t))

(use-package itail
  :ensure t
  :defer t)

(use-package jabber
  :ensure t
  :preface
  (define-key ctl-x-map "\C-j" jabber-global-keymap)
  :bind (("C-x C-j C-c" . jabber-connect-all)
         ("C-x C-j C-d" . jabber-disconnect)
         ("C-x C-j C-r" . jabber-display-roster)
         ("C-x C-j C-m" . jabber-muc-join))
  :commands (jabber-connect jabber-connect-all)
  :config
  (use-package jabber-otr
    :ensure t
    :commands jabber-otr-encrypt
    :config
    (set-face-background 'jabber-otr-encrypted nil)
    (set-face-background 'jabber-otr-encrypted-sent nil)
    (set-face-background 'jabber-otr-encrypted-unverified nil))

  (defun periklis/jabber-alert-func (who oldstatus newstatus statustext)
    "Nil function for jabber alerts.")

  (custom-set-variables
   '(jabber-activity-count-in-title t)
   '(jabber-activity-make-strings 'jabber-activity-make-strings-shorten)
   '(jabber-auto-reconnect t)
   '(jabber-chat-buffer-format "%n")
   '(jabber-chat-buffer-show-avatar nil)
   '(jabber-connection-ssl-program nil)
   '(jabber-groupchat-buffer-format "%n")
   '(jabber-mode-line-mode t)
   '(jabber-muc-private-buffer-format "%g-%n")
   '(jabber-roster-buffer "roster")
   '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
   '(jabber-roster-show-title nil)
   '(jabber-roster-show-bindings nil)
   '(jabber-roster-sort-functions
     '(jabber-roster-sort-by-status
       jabber-roster-sort-by-displayname
       jabber-roster-sort-by-group))
   '(jabber-alert-presence-message-function
     'periklis/jabber-alert-func))

  (defun periklis/jabber-alert-echo (msg &optional arg)
    "Jabber alert echo function."
    (unless (minibuffer-prompt)
      (message "")))

  (define-jabber-alert echo
    "Show a message in the echo area"
    'periklis/jabber-alert-echo)

  (add-hook 'jabber-chat-mode-hook #'flyspell-mode)
  (add-hook 'jabber-chat-mode-hook #'goto-address)
  (add-hook 'jabber-chat-mode-hook #'abbrev-mode))

(use-package jasminejs-mode
  :ensure t
  :commands jasminejs-mode)

(use-package java
  :commands java-mode
  :config
  (use-package javadoc-lookup
    :ensure t
    :commands (javadoc-lookup)
    :config
    (custom-set-variables
     '(javadoc-lookup-completing-read-function #'completing-read))
    (global-set-key (kbd "C-c C-e j") 'javadoc-lookup))

  (add-hook 'java-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'java-mode-hook #'c-toggle-auto-newline)
  (add-hook 'java-mode-hook #'c-toggle-hungry-state)
  (add-hook 'java-mode-hook #'electric-indent-mode)
  (add-hook 'java-mode-hook #'electric-layout-mode)
  (add-hook 'java-mode-hook #'electric-pair-mode)
  (add-hook 'java-mode-hook #'flyspell-prog-mode)
  (add-hook 'java-mode-hook #'history-mode)
  (add-hook 'java-mode-hook #'subword-mode)
  (add-hook 'java-mode-hook #'wisent-java-default-setup)
  (add-hook 'java-mode-hook #'semantic-mode)
  (add-hook 'java-mode-hook #'yas-minor-mode))

(use-package jenkins
  :ensure t
  :commands jenkins)

(use-package json-mode
  :ensure t
  :commands json-mode
  :init
  (use-package json-reformat :ensure t :defer t)
  (use-package json-snatcher :ensure t :defer t))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.spec\\'" . js2-mode))
  :config
  (use-package js2-refactor :ensure t :commands js2-refactor-mode)
  (use-package tern         :ensure t)
  (use-package company-tern :ensure t :commands tern-mode)

  (add-hook 'js2-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'js2-mode-hook #'subword-mode)
  (add-hook 'js2-mode-hook #'electric-indent-mode)
  (add-hook 'js2-mode-hook #'electric-layout-mode)
  (add-hook 'js2-mode-hook #'electric-pair-mode)
  (add-hook 'js2-mode-hook #'flyspell-prog-mode)
  (add-hook 'js2-mode-hook #'history-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'tern-mode)
  (add-hook 'js2-mode-hook #'yas-minor-mode))

(use-package karma
  :ensure t
  :commands karma-mode)

(use-package macrostep
  :ensure t
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :ensure t
  :bind (("C-c m g" . magit-status))
  :commands projectile-vc
  :init
  (custom-set-variables
   '(magit-last-seen-setup-instructions "1.4.0")
   '(magit-diff-options '("-b")))
  :config
  (use-package gitconfig-mode   :ensure t)
  (use-package gitignore-mode   :ensure t)
  (use-package git-timemachine  :ensure t))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode))

(use-package multi-term
  :ensure t
  :demand t
  :config
  (use-package term+ :ensure t)
  (use-package helm-mt :ensure t :demand t)
  (custom-set-variables
   '(multi-term-program "~/.nix-profile/bin/zsh")
   '(multi-term-scroll-show-maximum-output nil)
   '(multi-term-scroll-to-bottom-on-output "this")
   '(multi-term-switch-after-close nil))
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))
  (add-hook 'term-setup-hook #'(lambda () (load-library "xterm-256color")))
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
  (add-to-list 'term-bind-key-alist '("C-f" . forward-char))
  (add-to-list 'term-bind-key-alist '("C-b" . backward-char))
  (add-to-list 'term-bind-key-alist '("M-f" . forward-word))
  (add-to-list 'term-bind-key-alist '("M-b" . backward-word))
  (add-to-list 'term-bind-key-alist '("C-a" . move-beginning-of-line))
  (add-to-list 'term-bind-key-alist '("C-e" . move-end-of-line)))

(use-package nix-mode
  :if nix-env-p
  :mode (("\\.nix\\'" . nix-mode)))

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

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (use-package orgit :ensure t :commands projectile-vc)
  (use-package org-mobile :demand t)
  (use-package org-protocol)
  (use-package org-projectile :ensure t)
  (use-package interleave :ensure t :commands interleave-mode)
  (use-package ox-pandoc :ensure t)

  (defun periklis/org-open-at-point (&optional arg)
    "Open link in external browser if ARG given."
    (interactive "P")
    (if (not arg)
        (let ((browse-url-browser-function #'browse-url-default-macosx-browser))
          (org-open-at-point))
      (org-open-at-point)))

  (define-key org-mode-map (kbd "C-c C-o") #'periklis/org-open-at-point)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (java . t)
     (js . t)
     (makefile . t)
     (shell . t)
     ))

  (custom-set-variables
   '(org-agenda-include-diary t)
   '(org-special-ctrl-a/e  t)
   '(org-tags-column -120))

  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'ord-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

(use-package pandoc-mode
  :ensure t
  :commands pandoc-mode
  :config
  (define-key 'pandoc-mode-map "C-c / r" #'pandoc-run-pandoc)
  (define-key 'pandoc-mode-map "C-c / p" #'pandoc-convert-to-pdf)
  (define-key 'pandoc-mode-map "C-c / s" #'pandoc-save-settings-file)
  (define-key 'pandoc-mode-map "C-c / w" #'pandoc-set-write)
  (define-key 'pandoc-mode-map "C-c / f" #'pandoc-set-master-file)
  (define-key 'pandoc-mode-map "C-c / m" #'pandoc-set-metadata)
  (define-key 'pandoc-mode-map "C-c / v" #'pandoc-set-variable)
  (define-key 'pandoc-mode-map "C-c / V" #'pandoc-view-output)
  (define-key 'pandoc-mode-map "C-c / S" #'pandoc-view-settings)
  (define-key 'pandoc-mode-map "C-c / c" #'pandoc-insert-@)
  (define-key 'pandoc-mode-map "C-c / C" #'pandoc-select-@))

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (custom-set-variables
   '(paradox-github-token t))
  (paradox-enable))

(use-package paredit
  :ensure t
  :disabled
  :commands paredit-mode
  :diminish paredit-mode
  :config
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

(use-package perspeen
  :ensure t
  :demand t
  :config
  (custom-set-variables
   '(perspeen-use-tab nil))
  (bind-key "C-z C-d" 'perspeen-tab-del perspeen-mode-map)
  (bind-key "C-z C-n" 'perspeen-tab-next perspeen-mode-map)
  (bind-key "C-z C-p" 'perspeen-tab-prev perspeen-mode-map)
  (perspeen-mode))

(use-package pdf-tools
  :ensure t
  :disabled
  :mode "\\.pdf\\'"
  :config
  (pdf-tools-install)
  (pdf-tools-enable-minor-modes))

(use-package projectile
  :ensure t
  :demand t
  :init
  ;; (use-package perspective      :ensure t :demand t)
  ;; (use-package persp-projectile :ensure t :demand t)
  :config
  (custom-set-variables
   '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
   '(projectile-mode-line-lighter "")
   '(projectile-enable-caching t)
   '(projectile-completion-system 'helm))

  (defun projectile-helm-ag ()
    (interactive)
    (helm-ag (projectile-project-root)))

  (projectile-mode)
  ;; (persp-mode)
  (helm-projectile-on))

(use-package php-mode
  :ensure t
  :commands (php-mode)
  :config
  (use-package inf-php             :ensure t :commands inf-php)
  (use-package phpcbf              :ensure t :commands php-mode)
  (use-package php-extras          :ensure t :commands php-mode)
  (use-package php-refactor-mode   :ensure t :commands php-mode)
  (use-package php-auto-yasnippets :ensure t :commands php-mode)
  (use-package phpunit             :ensure t :commands php-mode)
  (load "semantic-php/loaddefs.el")

  (custom-set-variables
   '(php-mode-speedbar-open nil)
   '(php-refactor-command "refactor")
   '(phpunit-arg "")
   '(phpunit-program "phpunit --colors --disallow-test-output")
   '(phpunit-stop-on-error t)
   '(phpunit-stop-on-failure t)
   '(php-auto-yasnippet-php-program
     (expand-file-name
      "elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php"
      user-emacs-directory)))

  (defun periklis/setup-php-comany-backends ()
    "Setup company backends for php-mode."
    (set (make-local-variable 'company-backends) '(company-semantic company-gtags))
    (add-to-list 'company-semantic-modes 'php-mode))

  (add-hook 'php-mode-hook #'periklis/setup-php-comany-backends)
  (add-hook 'php-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'php-mode-hook #'electric-indent-mode)
  (add-hook 'php-mode-hook #'electric-layout-mode)
  (add-hook 'php-mode-hook #'electric-pair-mode)
  (add-hook 'php-mode-hook #'c-toggle-auto-newline)
  (add-hook 'php-mode-hook #'c-toggle-hungry-state)
  (add-hook 'php-mode-hook #'flyspell-prog-mode)
  (add-hook 'php-mode-hook #'helm-gtags-mode)
  (add-hook 'php-mode-hook #'history-mode)
  (add-hook 'php-mode-hook #'php-refactor-mode)
  (add-hook 'php-mode-hook #'semantic-php-default-setup)
  (add-hook 'php-mode-hook #'semantic-mode)
  (add-hook 'php-mode-hook #'subword-mode)

  ;; key bindings
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
  (define-key php-mode-map (kbd "C-x s") 'company-semantic)
  (define-key php-mode-map (kbd "C-x t") 'phpunit-current-test)
  (define-key php-mode-map (kbd "C-x c") 'phpunit-current-class)
  (define-key php-mode-map (kbd "C-x p") 'phpunit-current-project))

(use-package puppet-mode
  :ensure t
  :mode ("\\.pp\\'" . puppet-mode))

(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :config
  (custom-set-variables
   '(restclient-log-request nil)))

(use-package sass-mode
  :ensure t
  :mode ("\\.scss\\'" . sass-mode))

(use-package semantic
  :commands (semantic-mode)
  :config
  ;; Enabe semenatic idle modes
  (add-to-list 'semantic-default-submodes 'semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'semantic-idle-local-symbol-highlight-mode)
  (add-to-list 'semantic-default-submodes 'semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'semantic-idle-summary-mode)

  ;; Enable semanticdb modes
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

  ;; Enable semenatic highlighting/bookmarking modes
  (add-to-list 'semantic-default-submodes 'semantic-highlight-edits-mode)
  (add-to-list 'semantic-default-submodes 'semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'semantic-idle-breadcrumbs-mode)

  ;; Enable semantic status modes
  (add-to-list 'semantic-default-submodes 'semantic-show-parser-state-mode))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (custom-set-variables
   '(sml/shorten-directory t)
   '(sml/shorten-modes t))
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package smartparens
  :ensure t
  :init
  (electric-pair-mode -1)
  (require 'smartparens-config)
  ;; Turn on smartparens in the minibuffer
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "M-r") 'sp-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)

  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-M-[") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

  ;; Handle backspace in c-like modes better for smartparens
  (bind-key [remap c-electric-backspace]
            'sp-backward-delete-char smartparens-strict-mode-map)

  ;; ;; Bind ";" to sp-comment in elisp
  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  (defun sp--org-skip-asterisk (ms mb me)
    (or (and (= (line-beginning-position) mb)
             (eq 32 (char-after (1+ mb))))
        (and (= (1+ (line-beginning-position)) me)
             (eq 32 (char-after me)))))

  ;; Org-mode
  (sp-with-modes
      'org-mode
    ;; (sp-local-pair "*" "*"
    ;;                :actions '(insert wrap)
    ;;                :unless '(sp-point-after-word-p sp-point-at-bol-p)
    ;;                :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

  ;;; Prog-modes
  (sp-with-modes
      '(java-mode c++-mode php-mode js2-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))

  (smartparens-global-strict-mode -1)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode -1))

(use-package srefactor
  :ensure t
  :commands (srefactor-refactor-at-point)
  :config
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

(use-package shell
  :ensure t
  :demand t
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    "Turns ascii colors in compilation buffer."
    (read-only-mode)
    (ansi-color-apply-on-region (point-min) (point-max)))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package ssh
  :ensure t
  :commands ssh
  :config
  (add-hook 'ssh-mode-hook
            (lambda ()
              (setq ssh-directory-tracking-mode t)
              (shell-dirtrack-mode t)
              (setq shell-dirtrackp nil))))

(use-package ssh-config-mode
  :ensure t
  :commands ssh-config-mode)

(use-package tide
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)
         ("\\.spec.ts\\'". typescript-mode))
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (eldoc-mode +1)))
  (add-hook 'typescript-mode-hook #'flyspell-prog-mode)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package tramp
  :demand t
  :config
  (use-package helm-tramp
    :demand t
    :config
    (defalias 'exit-tramp 'tramp-cleanup-all-buffers))

  (custom-set-variables
   '(tramp-default-method "ssh"))

  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(use-package twig-mode
  :ensure t
  :mode ("\\.twig\\'". twig-mode)
  :commands twig-mode)

(use-package undo-tree
  :ensure t
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package vagrant
  :ensure t
  :commands (vagrant-up vagrant-resume vagrant-suspend)
  :config
  (use-package vagrant-tramp :ensure t))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (custom-set-variables
   '(web-mode-enable-current-element-highlight nil)
   '(web-mode-enable-current-column-highlight nil))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  (custom-set-variables
   '(which-key-sort-order 'which-key-key-order-alpha)
   '(which-key-use-C-h-commands t)))

(use-package whitespace-cleanup-mode
  :ensure t
  :demand t
  :diminish whitespace-cleanup-mode
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package wget
  :ensure t
  :commands wget)

(use-package wgrep
  :ensure t
  :commands (grep rgrep)
  :config
  (custom-set-variables
   '(wgrep-auto-save-buffer t))
  (use-package wgrep-helm :ensure t :demand t)
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit))

(use-package with-editor
  :ensure t
  :init
  (progn
    (add-hook 'shell-mode-hook  'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package xterm-color
  :ensure t
  :demand t
  :config
  (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
         (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
         ;;(setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
         ))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook #'yas-minor-mode))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :diminish  yas-minor-mode
  :config
  (custom-set-variables
   '(yas-prompt-functions
     '(yas-completing-prompt))))

;; Add global key bindings
(use-package key-bindings
  :demand t)

;; Load local machine specific stuff
(defvar local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file)
  (load local-file))

;;; init.el ends here
