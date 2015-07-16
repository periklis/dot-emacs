;;; wisent-php.el --- Php LALR parser for Emacs

;; Copyright (C) 2008, 2009 Anonymous
;;
;; NOTE: Original author wished to remian anonymous and did not assign copyright
;;       to the FSF.

;; Author: Original code for Java by David Ponce <david@dponce.com>
;; Keywords: syntax

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(require 'semantic/wisent)
(require 'semantic/find)
(require 'semantic/db)
(require 'wisent-php-wy)
(eval-when-compile
  (require 'semantic/util)
  (require 'semantic/ctxt)
  (require 'semantic/imenu)
  (require 'semantic/senator))
(require 'semantic/bovine/c)

;;;;
;;;; Simple parser error reporting function
;;;;

(defun wisent-php-parse-error (msg)
  "Error reporting function called when a parse error occurs.
MSG is the message string to report."
;;   (let ((error-start (nth 2 wisent-input)))
;;     (if (number-or-marker-p error-start)
;;         (goto-char error-start)))
  (message msg)
  ;;(debug)
  )

;;;;
;;;; Local context
;;;;

(defun wisent-php-get-in-class-local-variables (tag)
  "Return a list of variable tags defined in a class' method.

TAG is a class tag."
  (list
   (semantic-tag-new-variable "$this" tag)
   (semantic-tag-new-variable "static" tag)
   (semantic-tag-new-variable "self" tag)
   (semantic-tag-new-variable "parent"
                              (car (semantic-tag-type-superclasses tag)))))

(defun wisent-php-get-in-function-local-variables (tag)
  "Return the local variables defined in a function.

TAG is a function tag."
  (append
   (plist-get (semantic-tag-attributes tag) :local-variables)
   (semantic-get-local-arguments)))

(define-mode-local-override semantic-get-local-variables
  php-mode ()
  "Get local values from a specific context.
Parse the current context for `field_declaration' nonterminals to
collect tags, such as local variables or prototypes.
This function override `get-local-variables'.

Add `$this', `static' and `self' if needed"
  (let ((vars '())
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil)
        (tags (semantic-find-tag-by-overlay)))
    (dolist (tag tags vars)
      (cond
       ((wisent-php-is-tag-class-like? tag)
        (setq vars (append (wisent-php-get-in-class-local-variables tag) vars)))
       ((equal 'function (semantic-tag-class tag))
        (setq vars (append (wisent-php-get-in-function-local-variables tag) vars)))))))

;;;;
;;;; Member protection
;;;;

(define-mode-local-override semantic-tag-protection
  php-mode (tag &optional parent)
  "Return protection information about TAG with optional parent."
  (let ((type-modifiers (semantic-tag-modifiers tag))
        (protection nil))
    (while (and type-modifiers (not protection))
      (let ((modifier (car type-modifiers)))
        (setq protection
              (cond ((string= "private" modifier)
                     'private)
                    ((string= "protected" modifier)
                     'protected)
                    ((string= "public" modifier)
                     'public)
                    (t nil))))
      (setq type-modifiers (cdr type-modifiers)))
    (or protection 'public)))

(define-mode-local-override semantic-find-tags-by-scope-protection
  php-mode (scopeprotection parent &optional table)
  "Find all tags accessible by SCOPEPROTECTION.

SCOPEPROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.  A hard-coded order is used to determine a match.
PARENT is a tag representing the PARENT slot needed for
`semantic-tag-protection'.
TABLE is a list of tags (a subset of PARENT members) to scan.  If TABLE is nil,
the type members of PARENT are used.
See `semantic-tag-protected-p' for details on which tags are returned.

This implementation is just there to override `semantic-find-tags-by-scope-protection-c-mode'."
  (semantic-find-tags-by-scope-protection-default scopeprotection parent table))

;;;;
;;;; Name splitting / unsplitting
;;;;

(define-mode-local-override semantic-analyze-split-name
  php-mode (name)
  "Split a tag NAME into a sequence.
Sometimes NAMES are gathered from the parser that are componded.
In PHP, \"foo\bar\" means :
  \"The class BAR in the namespace FOO.\"
Return the string NAME for no change, or a list if it needs to be split."
  (let ((ans (split-string name (regexp-quote "\\"))))
    (if (= (length ans) 1)
        name
    (delete "" ans))))

(define-mode-local-override semantic-analyze-unsplit-name
  php-mode (namelist)
  "Assemble a NAMELIST into a string representing a compound name.
In PHP, (\"foo\" \"bar\") becomes \"foo\\bar\"."
  (mapconcat 'identity namelist "\\"))

;;;;
;;;; Tag scoping
;;;;

(defun wisent-php-is-tag-class-like? (tag)
  "Return non-nil if a tag is a PHP class or interface, nil otherwise."
  (and (equal 'type (semantic-tag-class tag))
       (member (semantic-tag-type tag) '("class" "interface"))))

(defun wisent-php-is-tag-alias? (tag)
  "Return non-nil if a tag is a PHP alias, nil otherwise."
  (and (equal 'type (semantic-tag-class tag))
       (string= (semantic-tag-type tag) "use")))

(defun wisent-php-get-aliased-type (tag)
  "Return the name of the type aliased by the use TAG."
  (car (semantic-tag-type-superclasses tag)))

(defun wisent-php-is-tag-namespace? (tag)
  "Return non-nil if a tag is a PHP namespace, nil otherwise."
  (and (equal 'type (semantic-tag-class tag))
       (string= (semantic-tag-type tag) "namespace")))

(defun wisent-php-dealias-name (name)
  "Dealias the NAME by analyzing use statements."
  (let* ((aliases (wisent-php-find-alias-tags))
         (split-name (semantic-analyze-split-name name))
         (name-suffix (if (stringp split-name)
                          ""
                        (concat "\\" (semantic-analyze-unsplit-name
                                      (cdr split-name))))))
    (catch 'dealiased-name
      (dolist (alias aliases name)
        (when (string= (semantic-tag-name alias) name)
          (throw 'dealiased-name (concat (wisent-php-get-aliased-type alias) name-suffix)))))))

(defun wisent-php-get-full-name (name namespace-prefix)
  "Return the full name of the type NAME.

if NAME is absolute, just return NAME.
Otherwise, prefix it with NAMESPACE-PREFIX."
  (if (= (aref name 0) ?\\)
      name
    (let ((dealised-name (wisent-php-dealias-name name)))
      (if (string= dealised-name name)
          (concat namespace-prefix "\\" name)
        dealised-name))))

(defun wisent-php-get-tag-namespace-prefix (tag)
  "Return the namespace prefix for TAG.

It implies looking at TAG's parents to find a namespace and
returning its name."
  (catch 'prefix
    (while tag
      (when (wisent-php-is-tag-namespace? tag)
        (throw 'prefix (semantic-tag-name tag)))
      (setq tag (semantic-find-tag-parent-by-overlay tag)))
    ""))

(defun wisent-php-tag-has-bounds? (tag)
  "Return t if TAG has bounds, nil otherwise."
  (condition-case nil
      (semantic-tag-bounds tag)
    (error nil)))

(defun wisent-php-get-implicit-types-for-tag (tag)
  "Return all types TAG implicitly references."
  ;; Only import type for tags that have buffer information
  (when (wisent-php-tag-has-bounds? tag)
    (let ((tag-names '())
          (prefix (wisent-php-get-tag-namespace-prefix tag)))

      (cond
       ((and (equal 'variable (semantic-tag-class tag)) (stringp (semantic-tag-type tag)))
        (add-to-list 'tag-names (semantic-tag-type tag)))
       ;; For a class, implicit types are its parent types
       ((or (wisent-php-is-tag-class-like? tag) (wisent-php-is-tag-alias? tag))
        (setq tag-names (semantic-tag-type-superclasses tag))))

      ;; Finally, prefix relative types by the namespace
      (mapcar
       (lambda (name) (wisent-php-get-full-name name prefix))
       tag-names))))

(defun wisent-php-find-alias-tags (&optional table)
  "Return all tags in TABLE that represents a use statement."
  (let ((tags (semantic-find-tags-by-type "use" (or table (current-buffer))))
        (namespaces (semantic-find-tags-by-class 'type (or table (current-buffer)))))
    (dolist (cur namespaces)
      (setq tags
            (append tags
                    (semantic-find-tags-by-type "use"
                                                (semantic-tag-get-attribute cur :members)))))
    tags))

(defun wisent-php-create-include-tag-from-alias (tag)
  "Create an include tag from a use statement TAG if possible.

Return nil if it cannot."
  (let ((include-tag (semantic-tag-new-include (wisent-php-get-aliased-type tag) nil)))
    (semantic-tag-set-bounds include-tag
                             (semantic-tag-start tag)
                             (semantic-tag-end tag))
    include-tag))

;; Use ede-php-autoload to resolve include files if it is loaded.  Nothing
;; in this code should depend on ede-php-autoload, so this should be
;; refactored later.
(eval-after-load 'ede-php-autoload
  '(define-mode-local-override semantic-tag-include-filename php-mode
     (tag)
     "Return the name of the file for this include TAG if possible."
     (let* ((tag-name (semantic-tag-name tag))
            (qualified-name (wisent-php-get-full-name tag-name
                                                      (wisent-php-get-tag-namespace-prefix tag)))
            (current-project (ede-current-project))
            file-name)
       (if (and (ede-php-autoload-project-p current-project)
                (setq file-name (ede-php-autoload-find-class-def-file current-project
                                                                  qualified-name)))
           file-name
         tag-name))))

(define-mode-local-override semantic-find-tags-included
  php-mode (&optional table)
  "Create include tags from use statements."
  (delq nil (mapcar #'wisent-php-create-include-tag-from-alias
                    (wisent-php-find-alias-tags table))))

(defun wisent-php-import-implicit-types (tags)
  "Return a list of the tags implicitly references in items of TAGS.

TAGS is a list of semantic tags."
  (let ((tag-names '())
        (implicit-tags '())
        (find-results nil))
    (dolist (tag tags)
      (setq tag-names (append tag-names (wisent-php-get-implicit-types-for-tag tag))))

    (dolist (name tag-names implicit-tags)
      (when (stringp name)
        (setq find-results (semanticdb-find-tags-by-name name))
        (when (and (semanticdb-find-results-p find-results)
                   (> (semanticdb-find-result-length find-results) 0))
          (setq implicit-tags (append implicit-tags
                                      (list (car (semanticdb-find-result-nth find-results 0))))))))

    implicit-tags))

(define-mode-local-override semantic-ctxt-scoped-types
  php-mode (&optional point)
  "Return type names in scope at POINT.

Add types in function arguments."
  (when point (goto-char point))
  (let ((tags-overlay (semantic-find-tag-by-overlay))
        (local-variables (semantic-get-local-variables)))
    (wisent-php-import-implicit-types (append tags-overlay local-variables))))

;;;;
;;;; Semantic integration of the Php LALR parser
;;;;

;; Lexing
(defvar-mode-local php-mode semantic-lex-number-expression semantic-php-number-regexp)

(defvar-mode-local php-mode semantic-lex-analyzer #'wisent-php-lexer)

(defvar-mode-local php-mode semantic-lex-syntax-modifications '((?\\ ".")))

(defvar-mode-local php-mode semantic-lex-comment-regex "\\(/\\*\\|//\\|#\\)")

;; Parsing
(defvar-mode-local php-mode semantic-tag-expand-function #'wisent-php-expand-tag)

(defvar-mode-local php-mode semantic-type-relation-separator-character
  '("->" "::" "\\"))

(defvar-mode-local php-mode semantic-command-separation-character ";")

;; Navigation
(defvar-mode-local php-mode semantic-symbol->name-assoc-list-for-type-parts
  '((type     . "Classes")
    (variable . "Variables")
    (function . "Methods")))

(defvar-mode-local php-mode senator-step-at-tag-classes '(function variable))

(defvar-mode-local php-mode semantic-imenu-summary-function #'semantic-format-tag-prototype)

;; semanticdb
(defvar-mode-local php-mode semanticdb-find-default-throttle
  '(file unloaded system omniscience))

;;;###autoload
(defun wisent-php-default-setup ()
  "Hook run to setup Semantic in `php-mode'.
Use the alternate LALR(1) parser."
  (set (make-local-variable 'wisent-in-php) nil)
  (wisent-php-wy--install-parser)
  (setq
   imenu-create-index-function 'semantic-create-imenu-index

   semantic-symbol->name-assoc-list
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Includes")
             (package  . "Package")))
   ))
  ;; Setup phpdoc stuff
  ;;(semantic-php-doc-setup))

;;
;; Tag merging / expansion
;;

(defun wisent-php-translate-to-container-list (tags)
  "Translate TAGS to a list that can be consumed by the container tag.

If TAGS is a container tag, extracts its tags.

If TAGS is actually a single tag, wrap it in a list.

Otherwise, return it unchanged."
  (cond
   ((wisent-php-is-tag-container? tags)
    (wisent-php-get-container-tags tags))
   ((semantic-tag-p tags)
    (list tags))
   (t tags)))

(defun wisent-php-create-container-tag (&rest tag-collections)
  "Create a container tag for wrapping TAG-COLLECTIONS."
  (semantic-tag "" 'container
                :tags (apply #'append
                             (mapcar #'wisent-php-translate-to-container-list
                                                   tag-collections))))

(defun wisent-php-get-container-tags (tag)
  "Return tags wrapped in the container TAG."
  (plist-get (semantic-tag-attributes tag) :tags))

(defun wisent-php-is-tag-container? (tag)
  "Return t if TAG is a container, nil otherwise."
  (equal (semantic-tag-class tag) 'container))

(defun wisent-php-expand-tag-namespace (tag)
  "Expand namespace TAG into a list of equivalent type tags.

It basically splits namespace parts (\"\\\") into a hierarchy of
namespaces."
  (let* ((split-name (semantic-analyze-split-name (semantic-tag-name tag)))
         (names (if (stringp split-name) (list split-name) (reverse split-name)))
         (current-tag nil)
         (loop-tag nil)
         (region (semantic-tag-bounds tag))
         (region-start (car region))
         (region-end (cadr region)))
    (dolist (name names)
      (setq current-tag (semantic-tag-new-type name "namespace"
                                            (if (null current-tag)
                                                (semantic-tag-type-members tag)
                                              (list current-tag))
                                            nil))
      (semantic-tag-set-bounds current-tag region-start region-end))
    (list current-tag)))

(defun wisent-php-expand-tag-alias (tag)
  "Replace alias TAG with a type and an include tag."
  (let* ((name (semantic-tag-name tag))
         (def (semantic-tag-type tag))
        (def-file "")
        (region (semantic-tag-bounds tag))
        (clone (semantic-tag-new-type name "use" nil (list (concat "\\" def))))
        (start (car region))
        (end (cadr region))
        (xpand (list clone)))
    (semantic-tag-set-bounds clone start end)
    xpand))

(defun wisent-php-expand-tag-variable (tag)
  "Strip the heading $ from instance variables."
  (when (and (member "attribute" (semantic-tag-modifiers tag))
             (not (member "static" (semantic-tag-modifiers tag))))
    (semantic-tag-set-name tag (substring (semantic-tag-name tag) 1)))
  (list tag))

(defun wisent-php-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.

Expand multiple variable or alias declarations merged into a single tag."
  (cond
   ((equal (semantic-tag-class tag) 'using)
    (wisent-php-expand-tag-alias tag))
   ((equal (semantic-tag-class tag) 'variable)
    (wisent-php-expand-tag-variable tag))
   ((wisent-php-is-tag-container? tag)
    (apply #'append (mapcar #'wisent-cook-tag (wisent-php-get-container-tags tag))))
   ((wisent-php-is-tag-namespace? tag)
    (wisent-php-expand-tag-namespace tag))
   (t (list tag))))

;;;###autoload
(add-hook 'php-mode-hook #'wisent-php-default-setup)

(provide 'wisent-php)

;;; wisent-php.el ends here
