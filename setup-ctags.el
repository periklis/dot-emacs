;;; setup-ctags.el --- Ctags configuration

;;; Commentary:

;;; Ctags configuration

;;; Code:

(setq ctags-executable "/usr/local/bin/ctags")

(defun create-tags (languages options)
  "Create tags file."
  (interactive)
  (shell-command
   (format "%s --languages=%s --options=%s -e -R ." ctags-executable languages options)))

(defun create-project-tags (languages options-file-name)
  "Create tags for current project."
  (interactive "sLanguages: \nsOptions: ")
  (create-tags languages options-file-name)
  (message "Created language tags (%s) for current project" languages))

(provide 'setup-ctags)
;;; setup-ctags.el ends here
