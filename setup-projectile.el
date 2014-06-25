;;
;; Projectile configuration
;;
(require 'projectile)
(require 'helm-projectile)

;; Load projectile globaly
(projectile-global-mode)

;; Key binding for helm projectile search
(global-set-key (kbd "C-c h") 'helm-projectile)

(provide 'setup-projectile)
