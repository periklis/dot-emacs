;;
;; Emacs init configuration
;;
(setq inhibit-startup-message t)

;; Load backport setup if emacs version < 24.x
(load-file "~/.emacs.d/setup-backports.el")

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; Set path to themes
(setq custom-themes-dir (expand-file-name "themes" user-emacs-directory))

;; Set path for temporary directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
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
     ac-helm
     ac-js2
     ecb
     expand-region
     js2-mode
     js2-refactor
	 flx
     flymake-haskell-multi
     projectile
     haskell-mode
     helm
     helm-flymake
     helm-projectile
	 geben
     ghc
	 gitconfig-mode
	 gitignore-mode
     git-messenger
	 magit
	 magit-filenotify
	 ssh
	 ssh-config-mode
	 whitespace-cleanup-mode
     yasnippet
     php-auto-yasnippets)))

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

(require 'setup-yasnippet)
(require 'setup-auto-complete)
(require 'setup-expand-region)
(require 'setup-electric)
(require 'setup-shell)
(require 'setup-sr-speedbar)
(require 'setup-ctags)
(require 'setup-semantic)
(require 'setup-ecb)
(require 'setup-php-mode)
(require 'setup-flymake)
(require 'setup-flymake-phpcs)
(require 'setup-geben)
(require 'setup-haskell)
(require 'setup-magit)
(require 'setup-helm)
(require 'setup-projectile)
(require 'setup-ediff)
(require 'setup-js2-mode)

;; Add global key bindings
(require 'key-bindings)


