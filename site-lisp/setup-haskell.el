;;; setup-haskell.el --- Haskell configuration

;;; Commentary:

;;; Haskell configuration

;;; Code:

(setq cabal-lib-dir "~/.cabal/lib/")
(add-to-list 'load-path cabal-lib-dir)

;; Load cabal projects
(dolist (project (directory-files cabal-lib-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'ghc)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-mode-hook 'linum-mode)
(add-hook 'haskell-mode-hook 'electric-indent-mode)
(add-hook 'haskell-mode-hook 'electric-layout-mode)
(add-hook 'haskell-mode-hook 'electric-pair-mode)

;; Haskell mode identation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Haskell interactive mode setup
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-suggest-hoogle-imports t)
  '(haskell-process-log t))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; Haskell tags && stylish
(custom-set-variables
  '(haskell-tags-on-save t)
  '(haskell-stylish-on-save t))

(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)

(provide 'setup-haskell)
;;; setup-haskell.el ends here
