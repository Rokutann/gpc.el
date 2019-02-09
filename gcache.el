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


;; Core functions.

(defmacro gcache-defcache-spec (symbol doc-string spec)
  "Define a cache SPEC with DOC-STRING, and return SYMBOL.

This macro stores a marker in the property list of the
SYMBOL.  The format is '(gcache-cache spec . SYMBOL) .

SPEC should be a list of cache entry definitions, which is a list
of a cash entry, a default value, a retrieve function.

Example:
'((current-buffer-name \"*scratch*\" '(lambda () (buffer-name (current-buffer))))
                          (pwd \"/\" '(lambda () (with-temp-buffer
                                            (call-process \"pwd\" nil t)
                                            (buffer-string)))))"
  (declare (indent 1))
  `(prog1
       (defvar ,symbol nil ,doc-string)
     (put ',symbol 'gcache-cache-spec ',symbol)
     (setq ,symbol (make-hash-table))
     (mapcar #'(lambda (entry)
                 (puthash (car entry) (cdr entry) ,symbol))
             ,spec)))

(defmacro gcache-cache-spec-p (symbol)
  "Return t if SYMBOL is a cache spec, otherwise nil."
  (declare (indent 0))
  `(if (get ',symbol 'gcache-cache-spec)
       t
     nil))

(cl-defmacro gcache-defcache (symbol cache-spec &key buffer-local doc-string)
  "Define SYMBOL as a cache based on CASHE-SPEC, and return SYMBOL.

The new cache is automatically-buffer-local when BUFFER-LOCAL is
non-nil, otherwise global.

A cache is an alist with this structure:
\(\('a-cache-entry value . fetch-fucntion\)"
  `(prog1
       (defvar ,symbol nil ,doc-string)
     (gcache-util-copy-symbol-property 'gcache-cache-spec ',cache-spec ',symbol)
     (gcache--initialize-storage ,symbol)
     (when ,buffer-local
       (make-variable-buffer-local ',symbol))
     ))

(defmacro gcache--initialize-storage (symbol)
  "Initializse a storage for a cache and bind it to SYMBOL."
  `(setq ,symbol nil))


(defmacro gcache-set-default-content (cache)
  "Set the default content for CACHE from CACHE-SPEC."
  `(setq ,cache (gcache-util-make-alist-from-key-and-value0
                 (gcache-spec ,cache))))

(defmacro gcache-fetch (key cache)
  "Return the value of KEY in CACHE."
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
  "Clear all keys and values from CACHE."
  `(gcache-alist-clear ,cache))

(cl-defun gcache-keyp (key cache &key (testfn 'eq))
  "Return t if CACHE has ENTRY, otherwise nil."
  (if (gcache-alist-get key cache :testfn testfn) t nil))

(defun gcache-show (cache)
  "Show the content of CACHE and return it."
  (message (pp cache))
  cache)

(defmacro gcache-add (key fetch-fun cache)
  "Add KEY to CACHE with FETCH-FUN, and return ENTRY."
  `(unless (gcache-keyp ',key ,cache )
     (setq ,cache
           (push (cons ',key '(nil ,fetch-fun)) ,cache))
     ',key))

(defmacro gcache-remove (key cache)
  "Remove the pair with KEY from CACHE."
  `(gcache-alist-remove ,key ,cache))

;; Spec functions

(defmacro gcache-spec (cache)
  "Return the spec of CACHE."
  `(symbol-value (get ',cache 'gcache-cache-spec)))

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


;; Util functions.

(defun gcache-util-copy-symbol-property (propname from-symbol to-symbol)
  "Copy a property named PROPNAME from FROM-SYMBOL to TO-SYMBOL."
  (put to-symbol propname
       (get from-symbol propname)))

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


(provide 'gcache)
;;; gcache.el ends here
