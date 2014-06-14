;;
;; Ctags configuration
;;
(setq ctags-executable "/usr/local/bin/ctags")
(defun create-tags (dir-name tags-file-suffix languages)
  "Create tags file."
  (interactive)
  (shell-command
   (format "%s --languages=%s -e -f TAGS-%s -R %s/*" 
		   ctags-executable languages tags-file-suffix (directory-file-name dir-name))))

(defun create-project-tags (project-path project-name languages)
  "Create tags for current project."
  (interactive "DProject-Path: \nsProject-Name: \nsLanguages: ")
  (create-tags project-path (upcase project-name) languages)
  (message "Created language tags (%s) for project %s" languages project-name))

(defun insert-into-tags-table-list (e)
  (add-to-list 'tags-table-list e t))

(defun delete-from-tags-table-list (e)
  (setq tags-table-list (delete e tags-table-list)))

(defun create-project-tag-path (project-name)
  "Returns the full path to the project's tag file."
  (expand-file-name 
   (concat default-directory 
		   (format "/TAGS-%s" (upcase project-name)))))

(defun load-project-tags (project-name)
  "Loads the tags for project."
  (interactive "sProject: ")
  (let (project-path 
		project-tags)
	(setq project-path (create-project-tag-path project-name)) 
	(setq project-tags (list project-path))
	(mapc 'insert-into-tags-table-list project-tags)
	(message "Loaded tags for project %s" project-name)))

(defun unload-project-tags (project-name)
  "Unloads the tags for project."
  (interactive "sProject: ")
  (let (project-path 
		project-tags)
	(setq project-path (create-project-tag-path project-name)) 
	(setq project-tags (list project-path))
	(mapc 'delete-from-tags-table-list project-tags)
	(message "Unloaded tags for project %s" project-name)))

(provide 'setup-ctags)
