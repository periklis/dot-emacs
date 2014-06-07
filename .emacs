;;
;; Basic configuration
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'mccarthy t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq ident-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq major-modes-list '(emacs-lisp-mode lisp-mode php-mode javascript-mode))
(setq electric-ident-modes-list 'major-modes-list)
(setq electric-pair-modes-list 'major-modes-list)
(setq electric-layout-modes-list 'major-modes-list)

;;
;; Global key configuration
;;
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") 

;;
;; .emacs file handling
;;
(defun reload-dot-emacs ()
  "Reloads .emacs file"
  (interactive)
  (load-file "~/.emacs"))

;;
;; Speedbar sr-mode
;;
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-max-width 80)
(setq sr-speedbar-width-x 45)
(setq sr-speedbar-width-console 45)
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
(add-hook 'php-mode-hook (lambda () (linum-mode 1)))
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
 '(custom-safe-themes (quote ("bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
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

