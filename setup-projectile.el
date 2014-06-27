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
(global-set-key (kbd "C-c m") 'imenu-anywhere)
(global-set-key (kbd "C-c s") 'projectile-switch-project)
(global-set-key (kbd "C-c b") 'projectile-switch-to-buffer)

(provide 'setup-projectile)
