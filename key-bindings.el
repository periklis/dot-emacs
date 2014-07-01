;;
;; Global key configuration
;;

;; register mac specific keys for remote emacs session over ssh
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))

;; duplicate line command
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") 

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; magit
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

;; helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; helm-etags-plus
(global-set-key "\M-." 'helm-etags+-select)

;; helm auto-complete
(global-set-key (kbd "C-;") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-;") 'ac-complete-with-helm)

;; helm-projectile and projectile
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'imenu-anywhere)
(global-set-key (kbd "C-c s") 'projectile-switch-project)
(global-set-key (kbd "C-c b") 'projectile-switch-to-buffer)

(provide 'key-bindings)
