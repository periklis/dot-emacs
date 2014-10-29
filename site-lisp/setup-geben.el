;;; setup-geben.el --- Geben DGBP configuration

;;; Commentary:

;;; Geben DGBP configuration

;;; Code:
(require 'geben)

(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(setq geben-dbgp-default-proxy '("10.0.2.2" 9000 "PHPSTORM" nil t))

(provide 'setup-geben)
;;; setup-geben.el ends here
