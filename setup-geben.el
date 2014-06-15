;;
;; Geben DGP configuration
;;
(require 'geben)
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(setq geben-dbgp-default-proxy '("10.0.2.2" 9000 "EMACS-GEBEN" nil t))

(provide 'setup-geben)
