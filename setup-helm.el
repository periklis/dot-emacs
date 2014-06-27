;;
;; Helm configuration
;;
(require 'helm)

;; Load helm globaly
(helm-mode 1)

(global-set-key (kbd "M-.") 'helm-etags-select)

(provide 'setup-helm)
