;;; ede-php-root.el --- Simple EDE PHP Project

;; Copyright (C) 2014, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
;;         original code for C++ by Eric M. Ludlam <eric@siege-engine.com>
;; Keywords: PHP project ede

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
;; Simple PHP project for EDE.  Inspired by `ede-cpp-root-project'.
;;
;; Example project definition :
;; (ede-php-root-project "My project"
;;                       :file "/path/to/a/file/at/root"
;;                       :class-loaders '(:psr0 (("MyNs" . "src/MyNs")
;;                                               ("AnotherNs" . "src/AnotherNs"))
;;                                        :psr4 (("MyModernNs" . "src/modern/MyNs"))))
;;
(require 'ede)

;;; Code:

(defvar ede-php-root-project-list nil
  "List of projects created by otpion `ede-php-root-project'.")

(defun ede-php-root-file-existing (dir)
  "Find a php-root project in the list of php-root projects.
DIR is the drectory to search from."
  (let ((projs ede-php-root-project-list)
        (ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
        (when (string-match (concat "^" (regexp-quote root)) dir)
          (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

(defun ede-php-root-project-file-for-dir (&optional dir)
  "Return a full file name to the project file stored in DIR."
  (let ((proj (ede-php-root-file-existing dir)))
    (when proj (oref proj :file))))

;;;###autoload
(defun ede-php-root-project-root (&optional dir)
  "Get the root directory for DIR."
  (let ((projfile (ede-php-root-project-file-for-dir
                   (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

(defun ede-php-root-load (dir &optional rootproj)
  "Return a PHP root object if you created one.
Return nil if there isn't one.
DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (ede-php-root-file-existing dir))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "php-root"
                       :name "PHP ROOT"
                       :file 'ede-php-root
                       :proj-file 'ede-php-root-project-file-for-dir
                       :proj-root 'ede-php-root-project-root
                       :load-type 'ede-php-root-load
                       :class-sym 'ede-php-root-project
                       :new-p nil
                       :safe-p t)
 'unique)

;;;;
;;;; Class loaders
;;;;
;;;; In modern PHP, there is no thing like "#include" or "import".
;;;; The unknown classes are loaded at runtime using a custom loader.
;;;;
;;;; For example, with PSR-2 convention, to find the class \Bar\Foo
;;;; one have to search each include path to find the file Bar/Foo.php.

(defclass ede-php-root-class-loader ()
  ()
  "Base class for finding the file in with some class is defined."
  :abstract t)

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

CLASS-NAME must be the full name of the class, with all its parent namespaces."
  (error "Method `ede-php-root-find-class-def-file' must be overriden"))

;;;###autoload
(defclass ede-php-root-psr4-class-loader (ede-php-root-class-loader)
  ((namespaces :initarg :namespaces
                  :initform ()
                  :documentation
                  "An associative list in which keys are namespaces, and  values are their include paths.

For example, if :namespaces has the value '((\"Foo\" . \"src/Foo\") (\"Bar\" . \"src/test/Bar\")),
then The class \"Bar\\Foo\" is considered to be defined in \"src/test/Bar/Foo\"."))
  "Class loader for PSR-4 convention.")

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-psr4-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let* ((namelist (split-string class-name (regexp-quote "\\") t))
         (relative-path (concat (mapconcat 'identity (cdr namelist) "/") ".php"))
         (project-root (ede-project-root-directory (ede-current-project)))
         (namespaces (oref this namespaces))
         class-def-file)
    (while (and namespaces (not class-def-file))
      (let ((pair (car namespaces))
            (candidate-file ""))
        (when (string= (car namelist) (car pair))
          (setq candidate-file (expand-file-name relative-path
                                                 (expand-file-name (cdr pair)
                                                                   project-root)))
          (when (file-regular-p candidate-file)
            (setq class-def-file candidate-file)))
        (setq namespaces (cdr namespaces))))
    class-def-file))

;;;###autoload
(defclass ede-php-root-psr0-class-loader (ede-php-root-class-loader)
  ((namespaces :initarg :namespaces
                  :initform ()
                  :documentation
                  "An associative list in which keys are namespaces, and  values are their include paths.

For example, if :namespaces has the value '((\"Foo\" . \"src/Foo\") (\"Bar\" . \"src/test/Bar\")),
then The class \"Bar_Foo\" is considered to be defined in \"src/test/Bar/Foo\"."))
  "Class loader for PSR-0 convention.")

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-psr0-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let* ((namelist (split-string class-name (regexp-quote "_") t))
         (relative-path (concat (mapconcat 'identity (cdr namelist) "/") ".php"))
         (project-root (ede-project-root-directory (ede-current-project)))
         (namespaces (oref this namespaces))
         class-def-file)
    (while (and namespaces (not class-def-file))
      (let ((pair (car namespaces))
            (candidate-file ""))
        (when (string= (car namelist) (car pair))
          (setq candidate-file (expand-file-name relative-path
                                                 (expand-file-name (cdr pair)
                                                                   project-root)))
          (when (file-regular-p candidate-file)
            (setq class-def-file candidate-file)))
        (setq namespaces (cdr namespaces))))
    class-def-file))

(defclass ede-php-root-aggregate-class-loader (ede-php-root-class-loader)
  ((class-loaders :initarg :class-loaders
                  :initform ()
                  :documentation "The list of aggregated class loaders.

They must be instances of `ede-php-root-class-loader'."))
  "An aggregation of several class loaders.")

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-aggregate-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let ((loaders (oref this class-loaders))
        (class-def-file nil))
    (while (and loaders (not class-def-file))
      (setq class-def-file (ede-php-root-find-class-def-file (car loaders) class-name)
            loaders (cdr loaders)))
    class-def-file))

(defun ede-php-root-create-class-loader (conf)
  "Create a class loader from a configuration.

CONF is a property list.  Its keys are class norms, and its values
are the mappings between namespace and include path.

For example, the conf '(:psr4 ((\"Foo\" . \"src/Foo\") (\"Bar\"
\"src/test/Bar\"))) will create a class loader that will load
classes written with PSR-4 normal, mapping \"Foo\" and \"Bar\"
to the associated directories."
  (let ((loaders '())
        (load-config conf))
    (while load-config
      (let ((key (car load-config)))
        (cond
         ((equal key :psr0)
          (add-to-list 'loaders (ede-php-root-psr0-class-loader "PSR-0"
                                                                :namespaces (cadr load-config))))
         ((equal key :psr4)
          (add-to-list 'loaders (ede-php-root-psr4-class-loader "PSR-4"
                                                                :namespaces (cadr load-config)))))
        (setq load-config (cddr load-config))))
    (ede-php-root-aggregate-class-loader "Aggregate loader"
                                         :class-loaders loaders)))

(defclass ede-php-root-target (ede-target)
  ((project :initform nil
            :initarg :project))
  "EDE php-root project target.")

;;;###autoload
(defclass ede-php-root-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-php-root-project-list)
   (class-loader :initarg :class-loader
                 :type ede-php-root-class-loader
                 :documentation "The project's class loader.")))

(defmethod initialize-instance ((this ede-php-root-project) &rest fields)
  "Make sure the :file is fully expanded."
  (let ((class-autoloads (plist-get (car fields) :class-autoloads)))
    (call-next-method this (list
                            :file (plist-get (car fields) :file)
                            :class-loader (ede-php-root-create-class-loader class-autoloads))))
  (let ((f (expand-file-name (oref this :file))))
    ;; Remove any previous entries from the main list.
    (let ((old (eieio-instance-tracker-find (file-name-directory f)
                                            :directory
                                            'ede-php-root-project-list)))
      (when (and old (not (eq old this)))
        (delete-instance old)))
    ;; Basic initialization.
    (when (or (not (file-exists-p f))
              (file-directory-p f))
      (delete-instance this)
      (error ":file for ede-php-root-project must be a file"))
    (oset this :file f)
    (oset this :directory (file-name-directory f))
    (ede-project-directory-remove-hash (file-name-directory f))
    (ede-add-project-to-global-list this)
    (unless (slot-boundp this 'targets)
      (oset this :targets nil))))

(defmethod ede-find-subproject-for-directory ((proj ede-php-root-project) dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-find-target ((proj ede-php-root-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
         (dir default-directory)
         (ans (object-assoc dir :path targets)))
    (when (not ans)
      (setq ans (ede-php-root-target dir
                                     :name (file-name-nondirectory
                                            (directory-file-name dir))
                                     :path dir
                                     :source nil
                                     :project proj))
      (object-add-to-list proj :targets ans))
    ans))

(defmethod ede-project-root ((this ede-php-root-project))
  "Return my root."
  this)

(defmethod ede-project-root-directory ((this ede-php-root-project))
  "Return my root."
  (file-name-directory (oref this file)))

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-project) class-name)
  "Find the file in which CLASS-NAME is defined.

CLASS-NAME must be the full name of the class, with all its parent namespaces."
  (ede-php-root-find-class-def-file (oref this class-loader) class-name))

(provide 'ede-php-root)

;;; ede-php-root.el ends here
