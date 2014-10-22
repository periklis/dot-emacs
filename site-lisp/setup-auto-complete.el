;;; setup-auto-complete.el --- AC configuration

;;; Commentary:

;;; AC configuration

;;; Code:

(global-auto-complete-mode)

(setq ac-etags-requires 1)
(eval-after-load "etags"
  '(progn
    (ac-etags-setup)))

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here
