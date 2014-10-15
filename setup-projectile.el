;;; setup-projectile.el --- Projectile configuration

;;; Commentary:

;;; Projectile configuration

;;; Code:

(require 'projectile)
(require 'helm-projectile)
(require 'persp-projectile)

(setq projectile-tags-command "/usr/local/bin/ctags --languages=php --options=ctags.conf -e -R .")
(setq projectile-mode-line-lighter "")

;; Load projectile globaly
(projectile-global-mode)
(persp-mode)

(provide 'setup-projectile)
;;; setup-projectile.el ends here
