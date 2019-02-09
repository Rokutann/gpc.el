;;; gcache.el --- A general purpose cache fascility.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; URL: https://github.com/mukuge/el-cache
;; Keywords: lisp
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A general purpose cache fascility for Emaacs.

;;; Code:

(require 'cl-lib)

;; User facing variables.

(defvar gcache-namespace-polution nil
  "Polute the namespace if t, otherwise ensure all symbols begin with `gcache'.

Currently, `defcache' is the only polution this option introduces.")


;; Alist functions.

(defmacro gcache-alist-clear (alist)
  "Set ALIST nil."
  `(setq ,alist nil))

(cl-defmacro gcache-alist-set (key value alist &key (testfn 'eq))
  "Set a KEY VALUE pair in ALIST with TESTFN."
  `(setf (alist-get ,key ,alist nil nil ',testfn) ,value))

(cl-defmacro gcache-alist-remove (key alist &key (testfn 'eq))
  "Remove the pair with KEY in ALIST with TESTFN."
  `(setf (alist-get ,key ,alist nil t ',testfn) nil))


(cl-defun gcache-alist-get (key alist &key default (testfn 'eq))
  "Return the value of KEY in ALIST if exists TESTFN wise, otherwise DEFAULT."
  (alist-get key alist default nil testfn))

(defun gcache-alist-subset-p (alist-a alist-b)
  "Return t is ALIST-A is a sbuset of ALIST-B, otherwise nil."
  (let ((res t))
    (mapc #'(lambda (pair)
              (unless (member pair alist-b)
                (setq res nil)))
          alist-a)
    res))

(defun gcache-alist-set-equal (alist-a alist-b)
  "Return t if ALIST-A and ALIST-B are identical setwise, otherwise nil."
  (and (gcache-alist-subset-p alist-a alist-b)
       (gcache-alist-subset-p alist-b alist-a)))


;; Util functions.

(defun gcache-util-make-alist-from-key-and-value0 (hash)
  "Make and return an alist from keys and value0 of HASH."
  (let ((res nil))
    (maphash #'(lambda (key value)
                 (push (cons key (nth 0 value)) res))
             hash)
    res))

(defun gcache-util-hash-to-alist (hash)
  "Return an alist made from keys and values of HASH."
  (let ((res nil))
    (maphash #'(lambda (k v) (gcache-alist-set k v res)) hash)
    res))


;; Core functions.

(cl-defmacro gcache-defcache (symbol buffer-local doc-string &rest body)
  "Define SYMBOL as a cache, and return SYMBOL.

If the value of buffer-local is 'buffer-local, the cache is
automatically buffer-local. Otherwise, global.  The body defines
the spec of the cache. Each line of it consists of a key, an
initial value, and a fetch function.

Example:
(gcache-defcache g-cache 'global
    \"a global cache.\"
    (current-buffer-name \"*scratch*\" (lambda () (buffer-name (current-buffer))))
    (pwd \"/\" (lambda ()
               (with-temp-buffer
                 (call-process \"pwd\" nil t)
                 (s-chop-suffix \"\\n\" (buffer-string)))))
    (true nil a-retriever))"
  (declare (indent 2))
  `(prog1
       (defvar ,symbol nil ,doc-string)
     (let ((ahash (make-hash-table)))
       (mapcar #'(lambda (entry)
                   (puthash (car entry) (cdr entry) ahash))
               ',body)
       (put ',symbol 'gcache-cache-spec ahash))
     (gcache--initialize-storage ,symbol)
     (when (eq ,buffer-local 'buffer-local)
       (make-variable-buffer-local ',symbol))
     ))

(when gcache-namespace-polution
  (defalias 'defcache 'gcache-defcache))

(defmacro gcache--initialize-storage (symbol)
  "Initializse a storage for a cache and bind it to SYMBOL.

Currently, it just sets the SYMBOL to nil."
  `(setq ,symbol nil))

(defmacro gcache-set-default-content (cache)
  "Set the default content for CACHE from CACHE-SPEC.

Populate keys and initvalues from its cache spec."
  `(setq ,cache (gcache-util-make-alist-from-key-and-value0
                 (gcache-spec ,cache))))

(defmacro gcache-fetch (key cache)
  "Return the value of KEY in CACHE or with its fetch function."
  (let ((value (gensym))
        (new-value (gensym)))
    `(let ((,value (cdr (assoc ,key ,cache))))
       (if ,value
           ,value
         (let ((,new-value (funcall
                            (gcache-spec-get-fetchfn
                             ,key
                             ,cache))))
           (gcache-alist-set ,key ,new-value ,cache)
           ,new-value)))))

(defmacro gcache-clear (cache)
  "Clear all keys and values in CACHE."
  `(gcache-alist-clear ,cache))

(cl-defun gcache-keyp (key cache &key (testfn 'eq))
  "Return t if CACHE has an entry with KEY, otherwise nil."
  (if (gcache-alist-get key cache :testfn testfn) t nil))

(defun gcache-show (cache)
  "Show the content of CACHE and return it."
  (message (pp cache))
  cache)

(defmacro gcache-remove (key cache)
  "Remove the entry with KEY from CACHE."
  `(gcache-alist-remove ,key ,cache))

;; Spec functions

(defmacro gcache-spec (cache)
  "Return the spec of CACHE."
  `(get ',cache 'gcache-cache-spec))

(defmacro gcache-spec-show (cache)
  "Show the spec of CACHE, and return it."
  (let ((alist (gensym)))
    `(let ((,alist (gcache-util-hash-to-alist
                    (gcache-spec ,cache))))
       (message (pp ,alist))
       ,alist)))

(defmacro gcache-spec-set (key initval fetchfn cache)
  "Set a spec entry for CACHE with KEY, INITVAL, and FETCHFN."
  `(puthash ,key (list ,initval ,fetchfn) (gcache-spec ,cache)))

(defmacro gcache-spec-get (key cache)
  "Return the spec entry with KEY for CACHE."
  `(gethash ,key (gcache-spec ,cache)))

(defmacro gcache-spec-get-fetchfn (key cache)
  "Get the retrieve function for KEY for CACHE."
  `(nth 1 (gcache-spec-get ,key ,cache)))


(provide 'gcache)
;;; gcache.el ends here
