;;
;; Geben DGP configuration
;;
(require 'geben)
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(setq geben-dbgp-default-proxy '("127.0.0.1" 9000 "EMACS-GEBEN" nil t))

(provide 'setup-geben)
