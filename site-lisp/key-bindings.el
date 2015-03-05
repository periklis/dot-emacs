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
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h b") 'helm-descbinds)
(global-set-key (kbd "C-c h a") 'helm-ack)
(global-set-key (kbd "C-c h f") 'helm-apropos)
(global-set-key (kbd "C-c h d") 'helm-info-emacs)
(global-set-key (kbd "C-c h l") 'helm-locate-library)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
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
(global-set-key (kbd "C-c h p") 'helm-projectile)
(global-set-key (kbd "C-c p s") 'projectile-switch-project)
(global-set-key (kbd "C-c p b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c f e") 'helm-flycheck)
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

;; history-mode
(global-set-key (kbd "M-]") 'history-next-history)
(global-set-key (kbd "M-[") 'history-prev-history)

;; magit
(global-set-key (kbd "C-c m g") 'magit-status)

;;phpunit
(define-key php-mode-map (kbd "C-x t") 'phpunit-current-test)
(define-key php-mode-map (kbd "C-x c") 'phpunit-current-class)
(define-key php-mode-map (kbd "C-x p") 'phpunit-current-project)

(setq too-hardcore-backspace t)
(setq too-hardcore-return t)
(require 'hardcore-mode)
(global-hardcore-mode)

(provide 'key-bindings)
;;; key-bindings.el ends here
