;;; init.el --- Emacs initialization

;;; Commentary:

;;; Emacs initialization

;;; Code:

;; Improve startup time by gc-collect on after-init
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold
                  (car (get 'gc-cons-threshold 'standard-value)))))

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
  (let ((rtagsdir (expand-file-name "rtags" nix-site-lisp)))
    (when (file-accessible-directory-p rtagsdir)
      (add-to-list 'load-path rtagsdir))))

(when (eq window-system 'ns)
  ;; Unset TERM_PROGRAM=Apple_Terminal, which will be set if GUI Emacs was
  ;; launched from a terminal
  (setenv "TERM_PROGRAM" nil))

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
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

  (unless (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory))
    (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/melpa-stable" user-emacs-directory))
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (custom-set-variables
   '(use-package-verbose t)
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
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-environment '("TERM=xterm-256color"))
 '(display-battery-mode t)
 '(display-time-default-load-average 1)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(electric-pair-mode nil)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(fringe-mode '(4 . 0))
 '(global-linum-mode nil)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(load-prefer-newer t)
 '(max-lisp-eval-depth 50000)
 '(max-specpdl-size 5000)
 '(message-kill-buffer-on-exit t)
 '(network-security-level 'paranoid)
 '(next-error-recenter t)
 '(ns-auto-hide-menu-bar t)
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
 '(tls-checktrust t)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(winner-mode t))

;; Custom face definitions
(custom-set-faces
 '(hl-line ((t (:inherit highlight :background "#4E3D45" :underline nil)))))

;; Load Libraries
(use-package async                :ensure t :defer t)
(use-package bind-key             :ensure t :defer t)
(use-package dash                 :ensure t :defer t :config (eval-after-load "dash" '(dash-enable-font-lock)))
(use-package data-debug           :commands (data-debug-eval-expression))
(use-package deferred             :ensure t :demand t)
(use-package diminish             :ensure t :defer t)
(use-package duplicate-thing      :ensure t :bind ("C-c C-d" . duplicate-thing))
(use-package f                    :ensure t :defer t)
(use-package info+                :ensure t :commands (info))
(use-package let-alist            :ensure t :defer t)
(use-package popwin               :ensure t :demand t)
(use-package s                    :ensure t :defer t)
(use-package uuidgen              :ensure t :defer t)
(use-package xml-rpc              :ensure t :defer t)

;; Custom general hooks
(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'prog-mode-hook #'goto-address-mode)
(add-hook 'prog-mode-hook #'popwin-mode)
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'after-init-hook #'server-mode)

;; Load packages
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package alert
  :ensure t
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
  :disabled
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package bbdb
  :ensure t
  :demand t
  :config
  (use-package counsel-bbdb :ensure t)
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
      :disabled
      :if nix-env-p
      :config
      (custom-set-variables
       '(rtags-autostart-diagnostics t)
       '(rtags-completions-enabled t))

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
  :commands (create-tags create-project-tags)
  :config
  (defvar ctags-executable "~/.nix-profile/bin/ctags")

  (defun create-tags ()
    "Create tags file."
    (interactive)
    (shell-command
     (format "%s -e -R ." ctags-executable)))

  (defun create-project-tags ()
    "Create tags for current project."
    (interactive)
    (create-tags)
    (message "Created language tags for current project")))

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
  :disabled
  :ensure t
  :defer t
  :init
  (custom-set-variables
   '(solarized-termcolors 256))

  (defun periklis/load-solarized-theme ()
    "Load solarized theme."
    (load-theme 'solarized t)
    (set-frame-parameter nil 'background-mode 'dark)
    (set-terminal-parameter nil 'background-mode 'dark)
    (enable-theme 'solarized))

  (add-hook 'after-init-hook #'periklis/load-solarized-theme))

(use-package company
  :ensure t
  :demand t
  :diminish company-mode
  :bind (("C-c ;" . company-complete-common-or-cycle))
  :config
  (use-package company-c-headers :ensure t :defer t)
  (use-package company-quickhelp
    :ensure t
    :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
    :config (setq company-quickhelp-delay 2))
  (custom-set-variables
   '(company-auto-complete 'company-explicit-action-p)
   '(company-etags-everywhere t)
   '(company-dabbrev-downcase nil)
   '(company-idle-delay 0.2)
   '(company-minimum-prefix-length 1)
   '(company-require-match 'company-explicit-action-p)
   '(company-show-numbers t)
   '(company-tooltip-align-annotations t)
   '(company-tooltip-limit 10)
   '(company-tooltip-offset-display 'lines)
   '(company-transformers '(company-sort-by-backend-importance)))

  (add-hook 'after-init-hook 'global-company-mode))

(use-package darktooth-theme
  :ensure t
  :defer t
  :init
  (defun periklis/load-darktooth-theme ()
    "Load solarized theme."
    (load-theme 'darktooth t)
    (enable-theme 'darktooth))

  (add-hook 'after-init-hook #'periklis/load-darktooth-theme))


(use-package dired
  :commands (dired dired-jump)
  :config
  (custom-set-variables
   '(dired-dwim-target t)
   '(dired-ls-F-marks-symlinks t)
   '(diredp-hide-details-initially-flag nil)
   '(delete-by-moving-to-trash t)
   '(global-auto-revert-non-file-buffers t))

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


(use-package docker
  :ensure t
  :diminish docker-mode
  :config
  (docker-global-mode)
  (use-package docker-images)
  (use-package docker-containers)
  (use-package docker-volumes)
  (use-package docker-networks)
  (use-package docker-machine))

(use-package docker-compose-mode
  :ensure t
  :mode ("\\docker-compose.yml\\'" . docker-compose-mode))

(use-package ecb
  :ensure t
  :commands (ecb-activate ecb-deactivate)
  :config
  (custom-set-variables
   '(ecb-auto-update-methods-after-save t)
   '(ecb-compile-window-height 10)
   '(ecb-compile-window-temporally-enlarge 'after-selection)
   '(ecb-enlarged-compilation-window-max-height 'half)
   '(ecb-force-reparse-when-semantic-idle-scheduler-off t)
   '(ecb-layout-name "left2")
   '(ecb-methods-menu-sorter nil)
   '(ecb-non-semantic-exclude-modes (quote (sh-mode fundamental-mode text-mode)))
   '(ecb-options-version "2.50")
   '(ecb-post-process-semantic-taglist
     (quote
      ((c++-mode ecb-group-function-tags-with-parents)
       (emacs-lisp-mode ecb-group-function-tags-with-parents)
       (c-mode ecb-filter-c-prototype-tags))))
   '(ecb-scroll-other-window-scrolls-compile-window t)
   '(ecb-source-path (quote (("/" "/"))))
   '(ecb-tip-of-the-day nil)
   '(ecb-windows-width 45)
   '(ecb-major-modes-show-or-hide '((php-mode js2-mode haskell-mode))))

  (defun ecb-not-using-layout-advices (&rest args)
    "Do not use the layout advices of ecb."
    (ecb-disable-advices 'ecb-layout-basic-adviced-functions))
  (advice-add #'ecb-activate--impl :after #'ecb-not-using-layout-advices))

(use-package ediff
  :defer t
  :config
  (add-hook 'ediff-load-hook 'ecb-deactivate)
  (add-hook 'ediff-quit-hook 'ecb-activate)

  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-vertically)
  (setq ediff-ignore-similar-regions t))

(use-package elm-mode
  :ensure t
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (use-package elm-yasnippets :ensure t)
  (use-package flycheck-elm
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))

  (custom-set-variables
   '(elm-tags-exclude-elm-stuff nil)
   '(elm-format-on-save t)
   '(elm-sort-imports-on-save t)
   '(elm-tags-on-save nil))

  (defun periklis/elm-company-setup ()
    "Adds company-elm to company-backends."
    ;; (setq company-backends '(company-elm))
    (add-to-list 'company-backends 'company-elm))

  (add-hook 'elm-mode-hook #'periklis/elm-company-setup)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(use-package emacs-eclim
  :ensure t
  :disabled
  :commands (global-eclim-mode))

(use-package emacs-lisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'emacs-lisp-mode-hook #'electric-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'electric-layout-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
  (add-hook 'emacs-lisp-mode-hook #'subword-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'after-save-hook 'check-parens nil t))

(use-package ensime
  :ensure t
  :config
  (use-package sbt-mode :ensure t)
  (use-package scala-mode
    :ensure t
    :config
    (add-hook 'scala-mode-hook #'auto-revert-mode))
  (custom-set-variables
   '(ensime-startup-notification nil)
   '(ensime-startup-snapshot-notification nil)
   '(ensime-startup-snapshot-notification-3)
   '(ensime-overlays-use-font-lock t)
   '(ensime-eldoc-hints 'all)
   '(ensime-search-interface 'ivy)
   '(ensime-sem-high-faces
     '((var . scala-font-lock:var-face)
       (val . (:inherit font-lock-constant-face :slant italic))
       (varField . scala-font-lock:var-face)
       (valField . (:inherit font-lock-constant-face :slant italic))
       (functionCall . font-lock-function-name-face)
       (operator . font-lock-keyword-face)
       (param . (:slant italic))
       (class . font-lock-type-face)
       (trait .  (:inherit font-lock-type-face :slant italic))
       (object . font-lock-constant-face)
       (package . font-lock-preprocessor-face)
       (implicitConversion . nil)
       (implicitParams . nil)
       (deprecated . (:strike-through "dark gray"))))
   '(sbt:program-options '("-Djline.terminal=auto"))
   '(sbt:scroll-to-bottom-on-output t)))

(use-package erc
  :commands (erc erc-tls)
  :config
  (custom-set-variables
   '(erc-join-buffer 'bury))
  (erc-spelling-mode))

(use-package eshell
  :config
  (use-package em-smart)
  (defun periklis/eshell-mode-hook ()
    "Append visual commands to eshell mode hook."
    (add-to-list 'eshell-visual-commands "htop" t)
    (add-to-list 'eshell-visual-commands "fzf" t)
    (add-to-list 'eshell-visual-commands "nix-shell" t))

  (custom-set-variables
   '(eshell-where-to-jump 'begin)
   '(eshell-review-quick-commands nil)
   '(eshell-smart-space-goes-to-end t))

  (add-hook 'eshell-mode-hook #'periklis/eshell-mode-hook))

(use-package eslintd-fix
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook 'eslintd-fix-mode))

(use-package eyebrowse
  :ensure t
  :demand t
  :bind (("s-n" . eyebrowse-next-window-config)
         ("s-p" . eyebrowse-prev-window-config))
  :config
  (custom-set-variables
   '(eyebrowse-new-workspace t)
   '(eyebrowse-wrap-around t))
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)))

(use-package ffap
  :bind (("C-x C-f" . find-file-at-point)
         ("C-x C-r" . ffap-read-only)
         ("C-x C-v" . ffap-alternate-file)
         ("C-x 4 f" . ffap-other-window)
         ("C-x 5 f" . ffap-other-frame)
         ("C-x 4 r" . ffap-read-only-other-window)
         ("C-x 5 r" . ffap-read-only-other-frame)
         ("C-x d"  . dired-at-point)
         ("C-x 4 d" . ffap-dired-other-window)
         ("C-x 5 d" . ffap-dired-other-frame)
         ("C-x C-d" . ffap-list-directory))
  :ensure nil)

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
  (require 'gnus-demon)
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

(use-package groovy-mode
  :ensure t
  :config
  (use-package groovy-imports
    :ensure t))

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
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h c" . helpful-command)
         ("C-h C-h" . helpful-at-point)))

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

(use-package hippie-exp
  :bind (("M-/" . hippie-expand))
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
  :commands (geben))

(use-package import-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'run-import-js)
  (add-hook 'web-mode-hook #'run-import-js))

(use-package ibuffer
  :config
  (add-hook 'ibuffer-hook #'ibuffer-do-sort-by-alphabetic))

(use-package itail
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :demand t
  :bind (("C-'" . ivy-avy)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c h a" . counsel-ag)
         ("C-c h b" . counsel-descbinds)
         ("C-c h g" . counsel-git-grep)
         ("C-c h l" . counsel-locate)
         ("C-c h s" . counsel-git)
         ("M-y" . counsel-yank-pop)
         ("M-i" . swiper)
         ("C-c M-i" . swiper-multi))
  :config
  (use-package ivy-rich :ensure t)
  (use-package smex :ensure t)
  (use-package swiper :ensure t)
  (custom-set-variables
   '(ivy-height 10)
   '(ivy-use-virtual-buffers t)
   '(ivy-count-format "%d/%d ")
   '(ivy-virtual-abbreviate 'full)
   '(ivy-rich-path-style 'abbrev)
   '(ivy-rich-switch-buffer-align-virtual-buffer t)
   '(ivy-initial-inputs-alist nil)
   '(ivy-re-builders-alist
     '((t . ivy--regex-fuzzy))))

  (ivy-set-actions
   'ivy-switch-buffer
   '(("k" kill-buffer "kill")
     ("r" ivy--rename-buffer-action "rename")))

  (ivy-set-display-transformer
   'ivy-switch-buffer
   'ivy-rich-switch-buffer-transformer)

  (ivy-mode))

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
  (add-hook 'java-mode-hook #'flyspell-prog-mode)
  (add-hook 'java-mode-hook #'subword-mode)
  (add-hook 'java-mode-hook #'wisent-java-default-setup)
  (add-hook 'java-mode-hook #'semantic-mode)
  (add-hook 'java-mode-hook #'yas-minor-mode))

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

  (add-hook 'js2-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'js2-mode-hook #'subword-mode)
  (add-hook 'js2-mode-hook #'flyspell-prog-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'yas-minor-mode))

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
   '(magit-diff-options '("-b"))
   '(magit-completing-read-function 'ivy-completing-read))
  :config
  (use-package gitconfig-mode   :ensure t)
  (use-package gitignore-mode   :ensure t)
  (use-package git-timemachine  :ensure t))

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package markdown-preview-mode
    :ensure t
    :config
    (add-to-list
     'markdown-preview-stylesheets
     "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css"))

  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode))

(use-package multi-term
  :ensure t
  :demand t
  :config
  (use-package term+ :ensure t)
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
  (add-to-list 'term-bind-key-alist '("C-e" . move-end-of-line))

  (defun periklis/term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map "\177" 'term-pager-back-page)))

(use-package nix-mode
  :if nix-env-p
  :ensure t
  :mode (("\\.nix\\'" . nix-mode)))

(use-package nix-sandbox
  :if nix-env-p
  :ensure t)

(use-package nlinum
  :ensure t
  :if (< emacs-major-version 26))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (expand-file-name "nov-save-place" user-emacs-directory)))

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
  :bind (("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cl" . org-store-link))
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
   '(org-M-RET-may-split-line '((default . nil)))
   '(org-agenda-include-diary t)
   '(org-special-ctrl-a/e  t)
   '(org-tags-column -120))

  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'ord-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

(use-package pandoc-mode
  :ensure t
  :commands pandoc-mode
  :bind (:map pandoc-mode-map
              ("C-c / r" . pandoc-run-pandoc)
              ("C-c / p" . pandoc-convert-to-pdf)
              ("C-c / s" . pandoc-save-settings-file)
              ("C-c / w" . pandoc-set-write)
              ("C-c / f" . pandoc-set-master-file)
              ("C-c / m" . pandoc-set-metadata)
              ("C-c / v" . pandoc-set-variable)
              ("C-c / V" . pandoc-view-output)
              ("C-c / S" . pandoc-view-settings)
              ("C-c / c" . pandoc-insert-@)
              ("C-c / C" . pandoc-select-@)))

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (custom-set-variables
   '(paradox-github-token t))
  (paradox-enable))

(use-package pdf-tools
  :ensure t
  :mode "\\.pdf\\'"
  :config
  (pdf-tools-install)
  (pdf-tools-enable-minor-modes))

(use-package projectile
  :ensure t
  :demand t
  :config
  (use-package counsel-projectile :ensure t)
  (custom-set-variables
   '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
   '(projectile-mode-line-lighter "")
   '(projectile-enable-caching t)
   '(projectile-completion-system 'ivy))

  (projectile-mode)
  (counsel-projectile-mode))

(use-package php-mode
  :ensure t
  :commands (php-mode)
  :config
  (use-package phpcbf              :ensure t :commands php-mode)
  (use-package php-refactor-mode   :ensure t :commands php-mode)
  (use-package php-auto-yasnippets :ensure t :commands php-mode)
  (use-package phpunit             :ensure t :commands php-mode)

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
  (add-hook 'php-mode-hook #'c-toggle-auto-newline)
  (add-hook 'php-mode-hook #'c-toggle-hungry-state)
  (add-hook 'php-mode-hook #'flyspell-prog-mode)
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

(use-package powerline
  :ensure t
  :demand t
  :config
  (use-package spaceline :ensure t :demand t)
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (custom-set-variables
   '(restclient-log-request nil)))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (use-package cargo :ensure t)
  (use-package toml-mode :ensure t)
  (use-package racer :ensure t)
  (use-package flycheck-rust :ensure t)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

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

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-(") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-)") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-[") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

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

  ;;; Prog-modes
  (sp-with-modes
      '(java-mode c++-mode php-mode js2-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
  :config
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

  (custom-set-variables
   '(shell-dirtrack-verbose nil))

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

(use-package tern
  :ensure t
  :config
  (use-package company-tern :ensure t)

  (defun periklis/setup-company-tern ()
    "Add company-tern to company-backends."
    (add-to-list 'company-backends 'company-tern))

  (add-hook 'js2-mode-hook #'tern-mode)
  (add-hook 'js2-mode-hook #'periklis/setup-company-tern)
  (add-hook 'web-mode-hook #'periklis/setup-company-tern))

(use-package tide
  :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'linum-mode)
  (add-hook 'typescript-mode-hook #'flyspell-prog-mode))

(use-package tramp
  :demand t
  :bind ("C-c s" . counsel-tramp)
  :config
  (use-package counsel-tramp :ensure t)
  (use-package docker-tramp :ensure t)
  (use-package tramp-term   :ensure t)
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)

  (custom-set-variables
   '(tramp-default-method "ssh")
   '(tramp-histfile-override nil)
   '(vc-ignore-dir-regexp
     (format "\\(%s\\)\\|\\(%s\\)"
             vc-ignore-dir-regexp
             tramp-file-name-regexp)))

  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(use-package undo-tree
  :ensure t
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (custom-set-variables
   '(web-mode-enable-current-element-highlight nil)
   '(web-mode-enable-current-column-highlight nil))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (tern-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup)))))

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
  :disabled
  :commands wget)

(use-package wgrep
  :ensure t
  :commands (grep rgrep)
  :config
  (custom-set-variables
   '(wgrep-auto-save-buffer t))
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
  :bind (:map yas-minor-mode-map
              ("C-c y" . yas-expand))
  :commands yas-minor-mode
  :diminish  yas-minor-mode
  :config
  (use-package auto-yasnippet :ensure t)
  (use-package yasnippet-snippets :ensure t)
  (custom-set-variables
   '(yas-prompt-functions
     '(yas-completing-prompt))))

;; Add global key bindings
(use-package key-bindings
  :demand t)

;; Load local machine specific stuff
(defvar common-file (expand-file-name "common.el" user-emacs-directory))
(when (file-exists-p common-file)
  (load common-file))

(defvar local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file)
  (load local-file))

;;; init.el ends here
