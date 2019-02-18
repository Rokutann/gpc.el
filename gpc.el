;;; gpc.el --- A general purpose cache facility.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; URL: https://github.com/mukuge/gpc.el
;; Keywords: lisp
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

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

;; A general purpose cache facility for Emacs.

;;; Code:

(require 'cl-lib)
(require 'nalist)

;; Utility functions.

(defun gpc-util-hash-to-alist (hash-table)
  "Return an alist made from keys and values of HASH-TABLE."
  (let ((alist nil))
    (maphash #'(lambda (k v) (nalist-set k v alist)) hash-table)
    alist))

(defun gpc-util-alist-to-hash (alist)
  "Return a hash table made of keys and values of ALIST."
  (let ((ht (make-hash-table)))
    (nalist-map #'(lambda (k v) (puthash k v ht)) alist)
    ht))

;; Initilization functions.

(defmacro gpc-init (symbol spec-list)
  "Initialize SYMBOL as a general purpose cache with SPEC-LIST.

A general purpose cache, or `gpc', is a cache facility which
enables you to store return values of fetch functions for future
reuse.  It's like memoization inside-out.

A cache entity in `gpc' is a data structure with a name, which
utilizes symbol's features to implement its mechanism between
cache and its specification.

It uses two places to store information: One is the ordinary
variable binding to the symbol, which keeps the content of cache,
the other is the symbol's property list, where the specification
of cache is associated with the key `gpc-cache-spec'.

As for the cache content, a gpc is just a named association list,
or `nalist'.  Most of the cache access functions in `gpc' is
actually aliases to the corresponding functions in nalist.'

A cache spec is a hash table whose keys are the keys of the cache
content, and the value associated with each key is in the
format (initval fetchfn), or a list of its initial value and
fetch function."
  (declare (indent 1))
  `(prog1
       (nalist-init ,symbol nil)
     (gpc-set-spec ,symbol (gpc-util-alist-to-hash ,spec-list))))

(defalias 'gpc-make-local-variable 'nalist-make-local-variable)

(defalias 'gpc-make-variable-buffer-local 'nalist-make-variable-buffer-local)

(defmacro gpc-overwrite-with-initvals (cache)
  "Overwrite the cache content with initvals in the CACHE's spec."
  `(maphash #'(lambda (k v)
                (nalist-set k (car v) ,cache))
            (gpc-get-spec ,cache)))

(cl-defmacro defcache (symbol buffer-local doc-string &rest spec-list)
  "Define SYMBOL as a general purpose cache or gpc, and return SYMBOL.

This macro uses `defvar' internally. So, the resulting variable
is special and can have a DOC-STRING.  It makes the variable
automatically buffer-local if BUFFER-LOCAL is :buffer-local,
otherwise global.

The cache is initialized as an automatically buffer-local
variable if the value of BUFFER-LOCAL is
'buffer-local. Otherwise, as a global variable defined by
`defvar'.

SPEC-LIST defines the specification of the cache: the initial
value and fetch function for each key.  See `gpc-init' for the
detail."
  (declare (indent 2))
  `(prog1
       (defvar ,symbol nil ,doc-string)
     (gpc-set-spec ,symbol (gpc-util-alist-to-hash ',spec-list))
     (nalist-init ,symbol nil)
     (when (eq ,buffer-local :buffer-local)
       (gpc-make-variable-buffer-local ,symbol))))

;; Cache spec access functions

(defmacro gpc-set-spec (symbol hash-table)
  "Set HASH-TABLE in SYMBOL's plist as a cache spec.

HASH-TABLE should contain a cache spec following the spec
description format.  See `gpc-init' for the detail."
  `(put ',symbol 'gpc-cache-spec ,hash-table))

(defmacro gpc-get-spec (cache)
  "Return the spec of CACHE.

A cache spec is a hash table."
  `(get ',cache 'gpc-cache-spec))

(defmacro gpc-spec-set-entry (key initval fetchfn cache)
  "Set the CACHE's spec entry whose key is KEY to have the value (INITVAL FETCHFN)."
  `(puthash ,key (list ,initval ,fetchfn) (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-entry (key cache)
  "Return the entry with KEY if it's in the CACHE's spec, otherwise nil."
  `(gethash ,key (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-initval (key cache)
  "Get the initval of the pair with KEY in the CACHE's spec."
  `(nth 0 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-get-fetchfn (key cache)
  "Get the fetch function of the pair with KEY in the CACHE's spec."
  `(nth 1 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-map (function cache)
  "Call function for all keys and values of the CACHE's spec.

The function should have three arguments, which are filled by this macro with a key, its initval, and its fetchfn in this order."
  `(maphash '(lambda (k v)
               (funcall ,function k (nth 0 v) (nth 1 v)))
            (gpc-get-spec ,cache)))

(defmacro gpc-spec-keyp (key cache)
  "Return t if KEY is a key in the CACHE's spec, otherwise nil."
  `(if (gpc-spec-get-entry ,key ,cache) t nil))

(defmacro gpc-pp-spec (cache)
  "Pretty print the spec of CACHE, and return it."
  (let ((alist (gensym)))
    `(let ((,alist (gpc-util-hash-to-alist
                    (gpc-get-spec ,cache))))
       (message (pp ,alist))
       ,alist)))

;; Cache content access functions

(defalias 'gpc-val 'nalist-get)

(defmacro gpc-fetch (key cache)
  "Fetch the value of KEY in CACHE with its fetch function.

It returns the value associated with KEY."
  `(nalist-set ,key (funcall (gpc-spec-get-fetchfn ,key ,cache)) ,cache))

(defmacro gpc-fetch-all (cache)
  "Fetch values of all keys in the CACHE's spec."
  `(gpc-spec-map '(lambda (k v f)
                    (gpc-fetch k ,cache))
                 ,cache))

(cl-defmacro gpc-get (key cache &key (force nil))
  "Return the value of KEY in CACHE by calling the fetchfn if needed."
  `(if ,force
       (gpc-fetch ,key ,cache)
     (if (gpc-pair-exist-p ,key ,cache)
         (gpc-val ,key ,cache)
       (gpc-fetch ,key ,cache))))

(defalias 'gpc-set 'nalist-set)

(defalias 'gpc-remove 'nalist-remove)

(defalias 'gpc-clear 'nalist-clear)

(defalias 'gpc-pairs 'nalist-pairs)

(defalias 'gpc-keys 'nalist-keys)

(defalias 'gpc-values 'nalist-values)

(cl-defun gpc-pair-exist-p (key cache &key (testfn 'eq))
  "Return t if CACHE has an entry with KEY, otherwise nil."
  (nalist-get key cache :default nil :testfn testfn))

(defun gpc-pp (cache)
  "Pretty print and return the content of CACHE."
  (message (pp cache))
  cache)

(provide 'gpc)
;;; gpc.el ends here
