;;
;; PHP-Mode defuns 
;;
(c-add-style
 "mo4"
 '((c-basic-offset . 4)
   (c-offsets-alist . ((arglist-cont . php-lineup-arglist)
                       (arglist-intro . php-lineup-arglist-intro)
                       (arglist-close . php-lineup-arglist-close)
                       (topmost-intro-cont . (first c-lineup-cascaded-calls
                                                    php-lineup-arglist-intro))
                       (brace-list-intro . +)
                       (brace-list-entry . c-lineup-cascaded-calls)
                       (case-label . 4)
                       (statement-case-intro . 4)
                       (defun-close . 0)
                       (defun-block-intro . +)
                       (knr-argdecl . [0])
                       (arglist-cont-nonempty . c-lineup-cascaded-calls)
                       (statement-cont . php-lineup-hanging-semicolon)))))

(defun php-enable-mo4-coding-style ()
  "Makes php-mode use coding styles that are preferable for working with mo4."
  (interactive)
  (setq indent-tabs-mode nil
        fill-column 120
        tab-width 4
        c-indent-comments-syntactically-p t
        require-final-newline t)
  (c-set-style "mo4")
  ;; Undo drupal coding style whitespace effects
  (setq show-trailing-whitespace nil)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

(add-hook 'php-mode-mo4-hook 'php-enable-mo4-coding-style nil t)

