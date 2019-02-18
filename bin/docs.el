;;; This script is based on docs.el in f.el.
;;; See: https://github.com/rejeep/f.el/blob/master/bin/docs.el

(require 'dash)
(require 's)
(require 'f)

(defvar gpc-root-path
  (expand-file-name ".." (file-name-directory load-file-name)))

(defvar gpc-lib-file
  (expand-file-name "gpc.el" gpc-root-path))

(defvar gpc-readme-file
  (expand-file-name "README.md" gpc-root-path))

(defvar gpc-readme-template
  (expand-file-name "README.md.tpl" gpc-root-path))

(defvar gpc-fn-doc-mapping (make-hash-table :test 'equal))

(require 'gpc gpc-lib-file)

(-map
 (lambda (lib)
   (when (equal (car lib) gpc-lib-file)
     (-select
      (lambda (alist)
        (when (and
               (listp alist)
               (equal (car alist) 'defun)
               (s-matches? "^gpc-[^-][a-z-]+\\??$" (symbol-name (cdr alist))))
          (puthash (symbol-name (cdr alist)) (documentation (cdr alist)) gpc-fn-doc-mapping)))
      (cdr lib))))
 load-history)

(let ((content (f-read gpc-readme-template)))
  (maphash
   (lambda (fn doc)
     (setq content (s-replace (concat "{{" fn "}}") doc content)))
   gpc-fn-doc-mapping)
  (f-write content 'utf-8 gpc-readme-file))
