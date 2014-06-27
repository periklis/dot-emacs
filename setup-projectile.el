;;
;; Projectile configuration
;;
(require 'projectile)
(require 'helm-projectile)

(setq projectile-tags-command "/usr/local/bin/ctags --languages=php --options=ctags.conf -e -R .")
(setq projectile-mode-line-lighter "")

;; Load projectile globaly
(projectile-global-mode)

;; Key binding for helm projectile search
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'projectile-find-tag)

(provide 'setup-projectile)
