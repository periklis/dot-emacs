;;
;; Projectile configuration
;;
(require 'projectile)
(require 'helm-projectile)

(setq projectile-tags-command "/usr/local/bin/ctags --languages=php --options=ctags.conf -e -R .")
(setq projectile-mode-line-lighter "")

;; Load projectile globaly
(projectile-global-mode)

(provide 'setup-projectile)
