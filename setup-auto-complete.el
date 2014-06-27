;;
;; Auto-Complete configuration
;;

(global-auto-complete-mode)

(setq ac-etags-requires 1)
(eval-after-load "etags"
  '(progn
    (ac-etags-setup)))

(provide 'setup-auto-complete)
