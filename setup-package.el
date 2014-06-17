;; Setup package-archives
(require 'package)
(require 'dash)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(unless (file-exists-p "~/.emacs.d/elpa/archives/gnu")
  (package-refresh-contents))

(unless (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
  (package-refresh-contents))

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(package-initialize)

(defun packages-install (packages)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(provide 'setup-package)
