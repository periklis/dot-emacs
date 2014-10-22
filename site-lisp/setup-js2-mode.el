;;; setup-js2-mode.el --- JS2 mode configuration

;;; Commentary:

;;; JS2 Mode configuration

;;; Code:

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook '(lambda () (subword-mode 1)))
(add-hook 'js2-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'js2-mode-hook '(lambda () (setq truncate-lines 0)))

(provide 'setup-js2-mode)
;;; setup-js2-mode.el ends here
