;;
;; Flymake phpcs configuration
;;

;; Customize the coding standard checked by phpcs
(setq flymake-phpcs-standard "MO4")
(setq flymake-phpcs-show-rule t)

(require 'flymake-phpcs)

(setq flymake-allowed-file-name-masks
      (quote
       (("\\.php[345s]?$" flymake-phpcs-init flymake-simple-cleanup flymake-get-real-file-name flymake-phpcs-init)
        ("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
        ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init)
        ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init)
        ("\\.js\\'" flymake-javascript-init) ("\\.css\\'" flymake-css-init)
        ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
        ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
        ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
        ("\\.tex\\'" flymake-simple-tex-init)
        ("\\.idl\\'" flymake-simple-make-init)
        ("\\.spec\\'" flymake-specfile-init)
        ("\\.po\\'" flymake-pofile-init))))

(provide 'setup-flymake-phpcs)
