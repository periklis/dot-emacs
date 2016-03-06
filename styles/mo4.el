;; (require 'php-mode)

;; (defvar mo4/c-style-definition '() "MO4 Programming Style")

;; (defun mo4/return-statement-add-newline-before ()
;;   "Add new line before return statement."
;;   (if (or (word-search-forward "return" (line-end-position) t)
;;           (word-search-backward "return" (line-beginning-position) t))
;;       (progn
;;         (beginning-of-line)
;;         (newline-and-indent)
;;         (end-of-line)))
;;   ;; Return nil, because we add no new lines after semicolon of comma
;;   nil)

;; (defun mo4/align-array-members-on-arrow-vertical ()
;;   "Aligns array members verticaly to their arrows."
;;   (save-excursion
;;     (let ((beg (search-backward "[" nil t))
;;          (end (search-forward "]" nil t)))
;;      (align-regexp beg end "\\(\\s-*\\)=>")))
;;   ;; Return nil, because we add no new lines after semicolon of comma
;;   nil)

;; (defun mo4/align-assignments-vertical ()
;;   "Align assignments verticaly."
;;   (interactive)

;;   (let* ((emptyLineRegex "\\(^\n\\)+")
;;          (assignRegex "=")
;;          (limitBeg
;;           (save-excursion
;;             (re-search-backward emptyLineRegex nil t)))
;;          (limitEnd
;;           (save-excursion
;;             (re-search-forward emptyLineRegex nil t)))
;;          (beg (re-search-backward assignRegex limitBeg t))
;;          (end (re-search-forward assignRegex nil t)))
;;     (align-regexp beg end "\\(\\s-*\\)="))
;;   ;; Return nil, because we add no new lines after semicolon of comma
;;   )

;; (setq
;;  mo4/c-style-definition
;;  '("php"
;;    (c-basic-offset . 4)
;;    (fill-column . 120)
;;    (require-final-newline . t)
;;    (c-tab-always-indent . t)
;;    (c-indent-comments-syntactically-p . t)
;;    (c-comment-only-line-offset . 4)
;;    (c-hanging-semi&comma-criteria . (mo4/return-statement-add-newline-before
;;                                      ;;mo4/align-assignments-vertical
;;                                      mo4/align-array-members-on-arrow-vertical
;;                                      ))
;;    (c-cleanup-list . (brace-else-brace
;;                       brace-elseif-brace
;;                       brace-catch-brace
;;                       comment-close-slash))
;;    (c-offsets-alist . ((arglist-close . php-lineup-arglist-close)
;;                        (arglist-cont . (first php-lineup-cascaded-calls 0))
;;                        (arglist-cont-nonempty . (first php-lineup-cascaded-calls c-lineup-arglist))
;;                        (arglist-intro . php-lineup-arglist-intro)
;;                        (case-label . +)
;;                        (class-open . -)
;;                        (comment-intro . 0)
;;                        (inlambda . 0)
;;                        (lambda-intro-cont . +)
;;                        (inline-open . 0)
;;                        (label . +)
;;                        (statement-cont . (first php-lineup-cascaded-calls php-lineup-string-cont +))
;;                        (substatement-open . 0)
;;                        (topmost-intro-cont . (first php-lineup-cascaded-calls +))))
;;    (c-echo-syntactic-information-p . t)))

;; (c-add-style
;;  "mo4-test"
;;  mo4/c-style-definition)
