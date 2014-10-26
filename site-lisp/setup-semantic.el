;;; setup-semantic.el --- CEDET semantic configuration

;;; Commentary:

;;; CEDET semantic configuration

;;; Code:

(require 'semantic)
(require 'semantic/ia)
(require 'semantic/db-ebrowse)
(require 'wisent-php)

(defun my-semantic-init-hook ()
  "Basic semantic init method to be added to language mode hooks."

  ;; Enabe idle semenatic modes
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

  ;; Enable semanticdb modes
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

  ;; Enable semenatic highlighting/bookmarking modes
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode-hook)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

  ;; Enable semantic status modes
  (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)

  (semantic-mode 1))

(provide 'setup-semantic)
;;; setup-semantic.el ends here
