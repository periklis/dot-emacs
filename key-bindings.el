;;
;; Global key configuration
;;

;; register mac specific keys for remote emacs session over ssh
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))

;; Duplicate line command
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") 

;; Helm commands
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(provide 'key-bindings)
