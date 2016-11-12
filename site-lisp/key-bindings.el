;;; key-bindings.el --- Global key configuration

;;; Commentary:

;;; Global key configuration

;;; Code:

;; register mac specific keys for remote emacs session over ssh
(when (equal system-type 'darwin)
  (setq mac-control-modifier 'control)
  (setq mac-command-modifier 'meta)
  (setq mac-function-modifier 'super))

(global-set-key (kbd "<C-tab>") #'other-window)

;; compile-command
(global-set-key (kbd "C-x C-m") 'compile)

;; auto-indent newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; company-complete-common-or-cycle
(global-set-key (kbd "C-;") 'company-complete-common-or-cycle)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Eval data expression
(global-set-key "\M-:" 'data-debug-eval-expression)

;; helm
(global-unset-key (kbd "C-x c"))

(custom-set-variables
 '(helm-command-prefix-key "C-c h"))

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h a") 'helm-ag)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h g") 'helm-git-grep)
(global-set-key (kbd "C-c h b") 'helm-descbinds)
(global-set-key (kbd "C-c h f") 'helm-apropos)
(global-set-key (kbd "C-c h d") 'helm-info-emacs)
(global-set-key (kbd "C-c h l") 'helm-locate-library)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; helm-projectile and projectile
(global-set-key (kbd "C-c h p") 'helm-projectile)
(global-set-key (kbd "C-c p s") 'projectile-switch-project)
(global-set-key (kbd "C-c p b") 'projectile-switch-to-buffer)

;; perspective mode
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)
(global-set-key (kbd "s-n") 'persp-next)
(global-set-key (kbd "s-p") 'persp-prev)

;; history-mode
(global-set-key (kbd "M-]") 'history-next-history)
(global-set-key (kbd "M-[") 'history-prev-history)

;; org-mode
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)

;; semantic
(global-set-key (kbd "M-RET") 'semantic-ia-fast-jump)

(provide 'key-bindings)
;;; key-bindings.el ends here
