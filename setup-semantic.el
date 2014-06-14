;;
;; CEDET semantic module configuration
;;
(defun semantic-init-hook ()
  (require 'semantic)
  (require 'semantic/ia)
  (require 'wisent-php)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (semantic-mode 1))

(provide 'setup-semantic)
