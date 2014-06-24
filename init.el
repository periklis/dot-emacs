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
  (load-theme 'mccarthy t))

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
   '(ecb
	 flx
	 flx-ido
     helm
     imenu-anywhere
	 geben
	 gitconfig-mode
	 gitignore-mode
     git-messenger
	 magit
	 magit-filenotify
	 ssh
	 ssh-config-mode
	 whitespace-cleanup-mode)))

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

(require 'setup-electric)
(require 'setup-shell)
(require 'setup-sr-speedbar)
(require 'setup-ctags)
(require 'setup-semantic)
(require 'setup-ecb)
(require 'setup-flymake)
(require 'setup-flymake-phpcs)
(require 'setup-php-mode)
(require 'setup-geben)
(require 'setup-magit)

;; Add global key bindings
(require 'key-bindings)

;; Load helm globaly
(helm-mode 1)
