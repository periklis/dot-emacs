;;
;; Projectile configuration
;;
(require 'projectile)
(require 'helm-projectile)
(require 'persp-projectile)

(setq projectile-tags-command "/usr/local/bin/ctags --languages=php --options=ctags.conf -e -R .")
(setq projectile-mode-line-lighter "")

;; Load projectile globaly
(projectile-global-mode)
(persp-mode)

(provide 'setup-projectile)
