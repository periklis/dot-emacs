;;
;; Flymake phpcs configuration
;;
(require 'flymake-phpcs)

;; Customize the coding standard checked by phpcs
(setq flymake-phpcs-standard "/usr/local/etc/php-code-sniffer/Standards/MO4")
(setq flymake-phpcs-show-rule t)

(provide 'setup-flymake-phpcs)
