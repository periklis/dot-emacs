;;
;; Emacs backports for versions < 24.x
;;

;; Adding backported emacs user dir var for emacs versions < 23.x
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which additional per-user Emacs-specific files are placed.
  Various programs in Emacs store information in this directory.
  Note that this should end with a directory separator.
  See also `locate-user-emacs-file'."))

;; Adding backported emacs user dir var for emacs versions < 23.x
(unless (boundp 'custom-theme-load-path)
  (defvar custom-theme-load-path (expand-file-name "themes" user-emacs-directory) "Directory for themes loading"))

(provide 'setup-backports)
