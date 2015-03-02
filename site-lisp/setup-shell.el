;;; setup-shell.el --- Shell configuration

;;; Commentary:

;;; Shell configuration

;;; Code:

(setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i"))

(setq my-tramp-ssh-completions '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

(mapc (lambda (method)
        (tramp-set-completion-function method my-tramp-ssh-completions))
      '("fcp" "rsync" "scp" "scpc" "scpx" "sftp" "ssh"))

(add-hook 'ssh-mode-hook (lambda ()
                           (shell-dirtrack-mode nil)
                           (setq dirtrackp nil)))

(tramp-get-completion-function "ssh")
(provide 'setup-shell)
;;; setup-shell.el ends here
