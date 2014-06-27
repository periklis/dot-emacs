;;
;; Projectile configuration
;;
(require 'projectile)
(require 'helm-projectile)

;; Load projectile globaly
(projectile-global-mode)

;; Key binding for helm projectile search
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'projectile-find-tag)

(provide 'setup-projectile)
