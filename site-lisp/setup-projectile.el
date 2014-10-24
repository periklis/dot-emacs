;;; setup-projectile.el --- Projectile configuration

;;; Commentary:

;;; Projectile configuration

;;; Code:

(require 'projectile)
(require 'helm-projectile)
(require 'persp-projectile)

(setq projectile-tags-command "/usr/local/bin/ctags --languages=php --options=ctags.conf -e -R .")
(setq projectile-mode-line-lighter "")
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)

;; Load projectile globaly
(projectile-global-mode)
(persp-mode)
(helm-projectile-on)

(provide 'setup-projectile)
;;; setup-projectile.el ends here
