;;; setup-ediff.el --- Ediff configuration

;;; Commentary:

;;; Ediff configuration

;;; Code:

(add-hook 'ediff-load-hook 'ecb-deactivate)
(add-hook 'ediff-quit-hook 'ecb-activate)

(setq ediff-split-window-function 'split-window-vertically)
(setq ediff-ignore-similar-regions t)

(provide 'setup-ediff)
;;; setup-ediff.el ends here
