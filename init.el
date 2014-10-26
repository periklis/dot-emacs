;;; init.el --- Emacs initiliazation

;;; Commentary:

;;; Emacs initialization

;;; Code:

(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; Set path to themes
(setq custom-themes-dir (expand-file-name "themes" user-emacs-directory))

;; Set path for temporary directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

;; Setup emacs 24 theme loading
(when (and (equal emacs-major-version 24) (equal system-type 'darwin))
  ;; Register default theme load path
  (add-to-list 'custom-theme-load-path custom-themes-dir)
  ;; Load default theme
  (load-theme 'solarized-light t))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Add package manager configuration
(require 'setup-package)

;; Install missing packages
(defun init--install-packages ()
  (packages-install
   '(auto-complete
     auto-complete-exuberant-ctags
     ac-etags
     ac-haskell-process
     ac-helm
     ac-js2
     bash-completion
     ecb
     expand-region
     jabber
     js2-mode
     js2-refactor
	 flx
     flx-ido
     flycheck
     flycheck-haskell
     google-translate
     hackernews
     projectile
     perspective
     persp-projectile
     haskell-mode
     helm
     helm-descbinds
     helm-flycheck
     helm-hoogle
     helm-git-grep
     helm-google
     helm-projectile
     ghc
	 gitconfig-mode
	 gitignore-mode
     inf-php
     itail
	 magit
	 magit-filenotify
     puppet-mode
     puppetfile-mode
	 ssh
	 ssh-config-mode
     sudo-ext
	 whitespace-cleanup-mode
     yasnippet
     php-mode
     php-auto-yasnippets
     sos
     twig-mode
     vagrant
     wget
     yaml-mode)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(eval-after-load "dash" '(dash-enable-font-lock))

;; Genereal requires
(require 'itail)
(require 'inf-php)
(require 'expand-region)
(require 'bash-completion)

;; Programming environment configs
(require 'setup-auto-complete)
(require 'setup-ctags)
(require 'setup-flycheck)
(require 'setup-semantic)
(require 'setup-ecb)
(require 'setup-yasnippet)

;; Programming languages configs
(require 'setup-php-mode)
(require 'setup-haskell)
(require 'setup-js2-mode)

;; Navigation/Project management configs
(require 'setup-helm)
(require 'setup-projectile)
(require 'setup-ediff)

;; External tools
(require 'setup-shell)
(require 'setup-magit)
(require 'setup-jabber)

;; Add global key bindings
(require 'key-bindings)

;; Add emacs hooks
(require 'setup-hooks)

;;; init.el ends here
