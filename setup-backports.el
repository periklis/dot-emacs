;;; setup-backports.el --- Emacs 24.x backported functions

;;; Commentary:

;;; Emacs 24.x backported functions

;;; Code:

;; Adding backported emacs user dir var for emacs versions < 23.x
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which additional per-user Emacs-specific files are placed.
  Various programs in Emacs store information in this directory.
  Note that this should end with a directory separator.
  See also `locate-user-emacs-file'."))

(provide 'setup-backports)
;;; setup-backports.el ends here
