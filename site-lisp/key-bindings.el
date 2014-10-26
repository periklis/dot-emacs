;;; setup-auto-complete.el --- Global key configuration

;;; Commentary:

;;; Global key configuration

;;; Code:

;; register mac specific keys for remote emacs session over ssh
(when (equal system-type 'darwin)
  ;;(setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq mac-function-modifier 'super))

;; auto-complete
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; auto-indent newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; duplicate line command
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") 

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h b") 'helm-descbinds)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; helm-etags-plus
(global-set-key "\M-." 'helm-etags+-select)
(substitute-key-definition 'find-tag 'helm-etags-select global-map)

;; helm auto-complete
(global-set-key (kbd "C-;") 'ac-complete-with-helm)

;; helm-projectile and projectile
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'imenu-anywhere)
(global-set-key (kbd "C-c s") 'projectile-switch-project)
(global-set-key (kbd "C-c b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c e") 'helm-flycheck)
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'key-bindings)
;;; key-bindings.el ends here
