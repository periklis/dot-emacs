;;
;; Flymake configuration
;;

(setq flymake-log-file-name (concat temporary-file-directory "/flymake.log"))
(setq flymake-log-level -1)
(setq flymake-cursor-auto-enable t)
(setq flymake-no-changes-timeout 5)
(setq flymake-max-parallel-syntax-checks 12)
(setq flymake-run-in-place nil)
(setq flymake-number-of-errors-to-display nil)

(provide 'setup-flymake)
