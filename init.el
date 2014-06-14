;;
;; Emacs init configuration
;;
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; Set path to themes
(setq themes-dir (expand-file-name "themes" user-emacs-directory)

;; Set path for temporary directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'custom-theme-load-path themes-dir)

;; Load default theme
(load-theme 'mccarthy t)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Add package manager configuration
(require 'setup-package)

;; Install missing packages
(defun init--install-packages ()
  (package-install
   '(ecb flx flx-ido gitconfig-mode gitignore-mode ssh ssh-config-mode whitespace-cleanup-mode)))

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

(eval-after-load 'flymake (require 'setup-flymake))
(eval-after-load 'flymake-phpcs (require 'setup-flymake-phpcs))
(eval-after-load 'php-mode (require 'setup-php-mode))
(require 'keybindings)
(require 'setup-sr-speedbar)
(require 'setup-ctags)
(require 'setup-semantic)
(require 'setup-ecb)
(require 'flymake-cursor)
