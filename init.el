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

(when (eq window-system 'ns)
  ;; Unset TERM_PROGRAM=Apple_Terminal, which will be set if GUI Emacs was
  ;; launched from a terminal
  (setenv "TERM_PROGRAM" nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

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

;; Buffers that I don't want popping up by default
(add-to-list
 'display-buffer-alist
 '("\\*Async Shell Command\\*.*" display-buffer-no-window))
(add-to-list
 'display-buffer-alist
 '("\\*system-packages\\*.*" display-buffer-no-window))

;; Add package manager configuration
(eval-and-compile
  (setq package-archives nil)

  ;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  ;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

  ;; (unless (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
  ;;   (package-refresh-contents))

  (unless (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory))
    (package-refresh-contents))

  ;; (unless (file-exists-p (expand-file-name "elpa/archives/melpa-stable" user-emacs-directory))
  ;;   (package-refresh-contents))

  ;; (unless (package-installed-p 'use-package)
  ;;   (package-install 'use-package))

  (package-initialize nil)

  (custom-set-variables
   '(use-package-verbose t)
   '(use-package-enable-imenu-support t))

  (require 'cl)
  (require 'use-package)
  (use-package use-package-ensure-system-package
    :ensure t
    :config
    (custom-set-variables
     '(system-packages-noconfirm t))))

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
 '(compilation-environment '("TERM=screen-256color"))
 '(display-battery-mode t)
 '(display-time-default-load-average 1)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(electric-pair-mode nil)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(fast-but-imprecise-scrolling t)
 '(fringe-mode '(4 . 0))
 '(global-display-line-numbers-mode nil)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(gnutls-verify-error nil)
 '(gnutls-min-prime-bits 1024)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 ;; '(jit-lock-defer-time 0.01)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(max-lisp-eval-depth 50000)
 '(max-specpdl-size 500000)
 '(message-kill-buffer-on-exit t)
 '(network-security-level 'paranoid)
 '(next-error-recenter t)
 '(ns-auto-hide-menu-bar t)
 '(proced-tree-flag t)
 '(redisplay-dont-pause t)
 '(ring-bell-function (quote ignore) t)
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(scroll-preserve-screen-position t)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(system-uses-terminfo nil)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tls-checktrust t)
 '(tooltip-mode nil)
 '(use-dialog-box nil)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(winner-mode t)
 '(xterm-query-timeout nil))

;; Load Libraries
(use-package async                :ensure t :defer t)
(use-package bind-key             :ensure t :defer t)
(use-package dash                 :ensure t :defer t :config (eval-after-load "dash" '(dash-enable-font-lock)))
(use-package data-debug           :commands (data-debug-eval-expression))
(use-package deferred             :ensure t :demand t)
(use-package diminish             :ensure t :defer t)
(use-package duplicate-thing      :ensure t :bind ("C-c C-d" . duplicate-thing))
(use-package f                    :ensure t :defer t)
(use-package let-alist            :ensure t :defer t)
(use-package popwin               :ensure t :demand t
  :config
  (custom-set-variables
   '(popwin:popup-window-width 100)
   '(popwin:popup-window-position 'right)))
(use-package s                    :ensure t :defer t)
(use-package uuidgen              :ensure t :defer t)
(use-package xml-rpc              :ensure t :defer t)

;; Custom general hooks
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
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
    (cmake-ide-setup)
    (remove-hook 'before-save-hook 'cide--before-save))

  (add-hook 'c-mode-common-hook #'flyspell-prog-mode)
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  (add-hook 'c-mode-common-hook #'google-make-newline-indent))

(use-package ctags
  :commands (create-tags create-project-tags)
  :config
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
   '(company-tooltip-limit 20)
   '(company-tooltip-offset-display 'lines)
   '(company-transformers '(company-sort-by-backend-importance)))

  (add-hook 'after-init-hook 'global-company-mode))

(use-package darktooth-theme
  :disabled
  :ensure t
  :demand t
  :init
  (defun periklis/load-darktooth-theme ()
    "Load darktooth theme."
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

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package docker
  :ensure t
  :diminish docker-mode
  :bind ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure t
  :mode ("\\docker-compose.yml\\'" . docker-compose-mode))

(use-package dockerfile-mode
  :ensure t)

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

(use-package erc
  :commands (erc erc-tls)
  :config
  (custom-set-variables
   '(erc-button-mode nil)
   '(erc-hide-timestamps nil)
   '(erc-join-buffer 'bury)
   '(erc-log-insert-log-on-open nil)
   '(erc-log-channels t)
   '(erc-log-channels-directory "~/.cache/erc/")
   '(erc-max-buffer-size 20000)
   '(erc-prompt-for-password nil)
   '(erc-save-buffer-on-part t))
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
  (add-hook 'web-mode-hook 'eslintd-fix-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")))

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
  (use-package flycheck-inline :ensure t :demand t)
  (custom-set-variables
   '(flycheck-display-errors-function nil)
   '(flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package git-gutter
  :ensure t
  :demand
  :config
  (custom-set-variables
   '(git-gutter:ask-p nil)
   '(git-gutter:hide-gutter t)
   '(git-gutter:window-width 2)
   '(git-gutter:verbosity 0))
  (global-git-gutter-mode))

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
  ;; (add-hook 'message-mode-hook 'turn-on-orgstruct)
  ;; (add-hook 'message-mode-hook 'turn-on-orgtbl)
  ;; (add-hook 'message-mode-hook 'turn-on-orgstruct++)

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

(use-package go-mode
  :ensure t
  :ensure-system-package
  (go
   go-bindata
   go-tools
   delve
   (errcheck . "go get github.com/kisielk/errcheck")
   (fillstruct . "go get github.com/davidrjenni/reftools/cmd/fillstruct")
   (go-outline . "go get github.com/lukehoban/go-outline")
   (go-symbolds . "go get github.com/newhook/go-symbols")
   (godef . "go get github.com/rogpeppe/godef")
   (golint . "go get golang.org/x/lint/golint")
   (gomodifytags . "go get github.com/fatih/gomodifytags")
   (gopkgs . "go get github.com/uudashr/gopkgs/cmd/gopkgs@latest")
   (gopls . "go get golang.org/x/tools/gopls@latest")
   (goreturns . "go get github.com/sqs/goreturns")
   (gotests . "go get github.com/cweill/gotests/gotests")
   (impl . "go get github.com/josharian/impl")
   (keyify . "go get honnef.co/go/tools/cmd/keyify")
   (rdeps . "go get honnef.co/go/tools/cmd/rdeps")
   (staticcheck . "go get honnef.co/go/tools/cmd/staticcheck")
   (structlayout . "go get honnef.co/go/tools/cmd/structlayout")
   (structlayout-optimize . "go get honnef.co/go/tools/cmd/structlayout-optimize")
   (structlayout-pretty . "go get honnef.co/go/tools/cmd/structlayout-pretty")
   (unconvert . "go get github.com/mdempsky/unconvert"))
  :config
  (use-package go-dlv :ensure t)
  (use-package go-errcheck :ensure t)
  (use-package go-fill-struct :ensure t)
  (use-package go-gen-test :ensure t)
  (use-package go-guru :ensure t)
  (use-package go-imenu :ensure t)
  (use-package go-impl :ensure t)
  (use-package go-imports :ensure t)
  (use-package go-projectile :ensure t)
  (use-package go-rename :ensure t)
  (use-package go-snippets :ensure t)
  (use-package gotest :ensure t)

  (custom-set-variables
   '(gofmt-command "goimports")
   '(godoc-and-godef-command "go doc")
   '(go-packages-function 'go-packages-go-list))

  ;; Temporary go-vet fix until https://github.com/flycheck/flycheck/pull/1548
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet))))

  (defun periklis/setup-go-mode ()
    "Extra setup for go-mode."
    (local-set-key (kbd "M-.") 'lsp-find-definition)
    (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
    (setq indent-tabs-mode nil)
    (whitespace-cleanup-mode nil)
    (eldoc-mode nil)
    (projectile-register-project-type 'gomake projectile-go-project-test-function
                                  :compile "go build"
                                  :test "go test ./..."
                                  :test-suffix "_test")
    (define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
    (define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
    (define-key go-mode-map (kbd "C-x p") 'go-test-current-project))

  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'subword-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook #'periklis/setup-go-mode)
  (add-hook 'go-mode-hook #'company-mode)
  (add-hook 'go-mode-hook #'go-eldoc-setup)
  (add-hook 'go-mode-hook #'go-imenu-setup)
  (add-hook 'go-mode-hook #'lsp-deferred))

(use-package google-translate
  :ensure t
  :defer t
  :commands google-translate-at-point)

(use-package groovy-mode
  :ensure t
  :config
  (use-package groovy-imports
    :ensure t))

(use-package gruvbox-theme
  :disabled
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

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

(use-package hcl-mode
  :ensure t)

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

(use-package ibuffer
  :config
  (add-hook 'ibuffer-hook #'ibuffer-do-sort-by-alphabetic))

(use-package ivy
  :ensure t
  :demand t
  :bind (("C-'" . ivy-avy)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c h b" . counsel-descbinds)
         ("C-c h g" . counsel-git-grep)
         ("C-c h i" . counsel-imenu)
         ("C-c h l" . counsel-locate)
         ("C-c h s" . counsel-git)
         ("C-c h r" . counsel-rg)
         ("M-y" . counsel-yank-pop)
         ("M-i" . swiper)
         ("C-c M-i" . swiper-multi))
  :config
  (use-package ivy-rich :ensure t)
  (use-package smex :ensure t)
  (use-package swiper :ensure t)
  (custom-set-variables
   '(ivy-height 10)
   '(ivy-use-virtual-buffers nil)
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

(use-package jinja2-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :commands json-mode
  :init
  (use-package json-reformat :ensure t :defer t)
  (use-package json-snatcher :ensure t :defer t))

(use-package js2-mode
  :ensure t
  :config
  (use-package js2-refactor :ensure t :commands js2-refactor-mode)

  (add-hook 'js2-mode-hook #'(lambda () (setq truncate-lines 0)))
  (add-hook 'js2-mode-hook #'subword-mode)
  (add-hook 'js2-mode-hook #'flyspell-prog-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'yas-minor-mode))

(use-package list-environment
  :commands (list-environment))

(use-package lsp-mode
  :ensure t
  :bind (("C-c C-l d" . lsp-ui-doc-show)
         ("C-c C-l h" . lsp-ui-doc-hide)
         ("C-c C-l f" . lsp-format-buffer)
         ("C-c C-l j" . lsp-find-definition)
         ("C-c C-l J" . lsp-ui-peek-find-definitions)
         ("C-c C-l n" . lsp-rename)
         ("C-c C-l r" . lsp-find-references)
         ("C-c C-l R" . lsp-ui-peek-find-references)
         ("C-c C-l s" . lsp-ui-sideline-mode)
         ("C-c C-l i" . lsp-ui-imenu))
  :config
  (use-package company-lsp :ensure t)
  (use-package lsp-ui :ensure t))

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
  (use-package gitignore-mode   :ensure t))

(use-package markdown-mode
  :ensure t
  :mode (("\\README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package markdown-preview-mode
    :ensure t
    :config
    (add-to-list
     'markdown-preview-stylesheets
     "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css"))

  (custom-set-variables
   '(markdown-command "marked"))

  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode))

(use-package nix-mode
  :ensure t
  :mode (("\\.nix\\'" . nix-mode))
  :config
  (custom-set-variables
   '(nix-indent-function #'nix-indent-line)))

(use-package nord-theme
  :disabled
  :ensure t
  :config
  (load-theme 'nord t))

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

(use-package one-themes
  :ensure t
  :init
  (load-theme 'one-dark t))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cl" . org-store-link))
  :config
  (use-package org-mobile :disabled)
  (use-package org-protocol)
  (use-package org-projectile :ensure t)
  (use-package interleave :ensure t :commands interleave-mode)
  (use-package ox-pandoc :ensure t)

  (defun periklis/org-open-at-point (&optional arg)
    "Open link in external browser if ARG given."
    (interactive "P")
    (if (not arg)
        (let ((browse-url-browser-function #'browse-url-default-browser))
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
   '(org-use-speed-commands t)
   '(org-tags-column -120))

  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'ord-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

(use-package pandoc-mode
  :ensure t
  :commands pandoc-mode
  :ensure-system-package (pandoc)
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

(use-package pinentry
  :disabled
  :ensure t
  :config
  (custom-set-variables
   '(epa-pinentry-mode 'loopback))
  (pinentry-start))

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
  (counsel-projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (use-package elpy :ensure t)
  (use-package jedi :ensure t)
  (use-package company-jedi :ensure t)
  (use-package py-autopep8 :ensure t)
  (use-package virtualenv :ensure t)
  (custom-set-variables
   '(elpy-rpc-backend "jedi")
     '(jedi:setup-keys t)
     '(jedi:complete-on-dot t)
     '(python-shell-interpreter "ipython")
     '(python-shell-interpreter-args "-i"))
  (elpy-use-ipython)
  (add-hook 'python-mode-hook #'jedi:setup)
  (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

(use-package realgud
  :commands (realgud))

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

(use-package sbt-mode
  :ensure t
  :commands (sbt-start sbt-command)
  :config
  (custom-set-variables
   '(sbt:program-options '("-Djline.terminal=auto"))
   '(sbt:scroll-to-bottom-on-output t))

  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (add-hook 'scala-mode-hook #'auto-revert-mode)
  (add-hook 'scala-mode-hook #'lsp))

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
  (add-hook 'typescript-mode-hook #'display-line-numbers-mode)
  (add-hook 'typescript-mode-hook #'flyspell-prog-mode)

  (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package typescript-mode
  :ensure t
  :mode (("\\.js\\'" . typescript-mode)
         ("\\.spec\\'" . typescript-mode)))

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
                (tide-mode))))

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
  (global-whitespace-cleanup-mode))

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
  :disabled
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
