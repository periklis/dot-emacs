;;
;; Basic configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(defun reload-dot-emacs ()
  "Reloads .emacs file"
  (interactive)
  (load-file "~/.emacs"))

;;
;; Speedbar sr-mode
;;
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-max-width 50)
(setq sr-speedbar-width-x 25)
(setq sr-speedbar-width-console 25)
(setq sr-speedbar-auto-refresh t)
(require 'sr-speedbar)

;;
;; Flymake configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flymake")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flymake-cursor")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flymake-phpcs")

(setq flymake-cursor-auto-enable t)
(setq flymake-no-changes-timeout 5)
(setq flymake-max-parallel-syntax-checks 8)
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq flymake-run-in-place nil)
(setq flymake-run-in-place nil)
(setq flymake-number-of-errors-to-display nil)
(setq flymake-phpcs-command "~/.emacs.d/site-lisp/emacs-flymake-phpcs/bin/flymake_phpcs")
(setq flymake-phpcs-standard "mo4-coding-standard")
(setq flymake-phpcs-show-rule t)

(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-phpcs)

;;
;; PHP-Mode configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/php-mode")
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
(require 'php-mode)

;;
;; Ctags configuration
;;
(setq ctags-executable "/usr/local/bin/ctags")
(defun create-tags (dir-name tags-file-suffix languages)
  "Create tags file."
  (interactive)
  (shell-command
   (format "%s --languages=%s -e -f TAGS-%s -R %s/*" 
		   ctags-executable languages tags-file-suffix (directory-file-name dir-name))))

(defun create-project-tags (project-path project-name languages)
  "Create tags for current project."
  (interactive "DProject-Path: \nsProject-Name: \nsLanguages: ")
  (create-tags project-path (upcase project-name) languages)
  (message "Created language tags (%s) for project %s" languages project-name))

(defun insert-into-tags-table-list (e)
  (add-to-list 'tags-table-list e t))

(defun delete-from-tags-table-list (e)
  (setq tags-table-list (delete e tags-table-list)))

(defun create-project-tag-path (project-name)
  "Returns the full path to the project's tag file."
  (expand-file-name 
   (concat default-directory 
		   (format "/TAGS-%s" (upcase project-name)))))

(defun load-project-tags (project-name)
  "Loads the tags for project."
  (interactive "sProject: ")
  (let (project-path 
		project-tags)
	(setq project-path (create-project-tag-path project-name)) 
	(setq project-tags (list project-path))
	(mapc 'insert-into-tags-table-list project-tags)
	(message "Loaded tags for project %s" project-name)))

(defun unload-project-tags (project-name)
  "Unloads the tags for project."
  (interactive "sProject: ")
  (let (project-path 
		project-tags)
	(setq project-path (create-project-tag-path project-name)) 
	(setq project-tags (list project-path))
	(mapc 'delete-from-tags-table-list project-tags)
	(message "Unloaded tags for project %s" project-name)))

;;
;; Emacs configuration
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(flymake-no-changes-timeout 5)
 '(global-visual-line-mode t)
 '(php-executable "/usr/local/bin/php")
 '(php-mode-speedbar-open nil)
 '(scroll-bar-mode nil)
 '(sr-speedbar-max-width 80)
 '(sr-speedbar-right-side nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

