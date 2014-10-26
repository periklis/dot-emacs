;;; setup-xml.el --- NXML mode configuration

;;; Commentary:

;;; NXML mode configuration

;;; Code:


(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(setq nxml-auto-insert-xml-declaration-flag t)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)
(setq nxml-sexp-element-flag t)

(provide 'setup-xml)
;;; setup-xml.el ends here
