;;
;; Global key configuration
;;

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; Duplicate line command
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") 

(provide 'key-bindings)
