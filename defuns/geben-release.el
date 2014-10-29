;;; geben-release.el --- Geben Release Functions

;;; Commentary:
;;; Geben session release functions

;;; Code:

(defun my-geben-release ()
  (interactive)
  (geben-stop)
  (dolist (session geben-sessions)
    (ignore-errors
      (geben-session-release session))))

;;; geben-release.el ends here
