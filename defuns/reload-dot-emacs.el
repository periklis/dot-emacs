;;
;; .emacs file handling
;;
(defun reload-dot-emacs ()
  "Reloads .emacs file"
  (interactive)
  (load-file "~/.emacs"))

(defun reload-emacs-init ()
  "Reloads init.el file"
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))
