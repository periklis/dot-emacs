;;; setup-auto-complete.el --- AC configuration

;;; Commentary:

;;; AC configuration

;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-helm)

(setq ac-auto-start nil)
(setq ac-auto-show-menu nil)
(setq ac-use-quick-help nil)
(setq ac-ignore-case t)

(global-auto-complete-mode)

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here
