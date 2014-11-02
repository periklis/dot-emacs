;;; setup-js2-mode.el --- JS2 mode configuration

;;; Commentary:

;;; JS2 Mode configuration

;;; Code:

(require 'ac-js2)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq ac-js2-evaluate-calls t)

(add-hook 'js2-mode-hook '(lambda () (setq truncate-lines 0)))
(add-hook 'js2-mode-hook 'subword-mode)
(add-hook 'js2-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'electric-indent-mode)
(add-hook 'js2-mode-hook 'electric-layout-mode)
(add-hook 'js2-mode-hook 'electric-pair-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

(provide 'setup-js2-mode)
;;; setup-js2-mode.el ends here
