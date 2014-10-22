;;; setup-jabber.el --- Jabber configuration

;;; Commentary:

;;; Jabber configuration

;;; Code:

(require 'jabber)

(add-hook 'jabber-chat-mode-hook 'goto-address)

(provide 'setup-jabber)
;;; setup-jabber.el ends here
