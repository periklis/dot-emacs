;;; setup-package.el --- Package manager configuration

;;; Commentary:

;;; Package manager configuration

;;; Code:

(require 'package)
(require 'dash)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
  (package-refresh-contents))

(unless (file-exists-p (expand-file-name "elpa/archives/marmalade" user-emacs-directory))
  (package-refresh-contents))

(unless (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory))
  (package-refresh-contents))

(package-initialize)

(defun packages-install (packages)
  "Install each item of the list PACKAGES."
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(provide 'setup-package)
;;; setup-package.el ends here
