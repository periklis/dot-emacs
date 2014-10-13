;;
;; jabber configuration
;;
(require 'jabber)

(add-hook 'jabber-chat-mode-hook 'goto-address)

(provide 'setup-jabber)
