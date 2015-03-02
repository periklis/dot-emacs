;;; setup-helm.el --- Helm configuration

;;; Commentary:

;;; Helm configuration

;;; Code:

(require 'helm-config)
(require 'helm)
(require 'helm-ack)
(require 'helm-descbinds)
(require 'helm-etags+)
(require 'helm-git-grep)

;; configuration
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-recentf-fuzzy-match              t
      helm-buffers-fuzzy-matching           t
      helm-locate-fuzzy-match               t
      helm-M-x-fuzzy-match                  t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-apropos-fuzzy-match              t)

;; helm-ack configs
(setq helm-ack-base-command "ack -H --nogroup")

;; Load helm globaly
(helm-mode 1)
(helm-descbinds-mode)
(helm-autoresize-mode 1)

(provide 'setup-helm)
;;; setup-helm.el ends here
