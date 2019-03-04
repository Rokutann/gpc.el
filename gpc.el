;;; gpc.el --- A general purpose cache facility.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; URL: https://github.com/mukuge/gpc.el
;; Keywords: lisp
;; Package-Version: 0.1.1.d
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

(defmacro gpc-init (name spec-list)
  "Bind NAME to a general purpose cache specified by SPEC-LIST.

General purpose cache, or `gpc', is a cache facility which stores
return values of fetch functions under a single NAME or variable.

`gpc' uses two places to store information: One is its ordinary
variable binding, which keeps cached data, the other is the
symbol's property list with the key `gpc-cache-spec', where the
specification of the cache is kept.

Aside from the cache spec mechanism, a gpc cache is just a named
association list, or `nalist'.  Some of its cache access
functions are actually aliases to the corresponding functions in
the `nalist' library.'

A cache spec is implemented as a hash table whose key corresponds
to the key of a cache entry one-to-one, and the value associated
with each key is a list, (initval fetchfn), which specifies the
initial value and fetch function of the cash entry."
  (declare (indent 1))
  `(prog1
       (nalist-init ,name nil)
     (gpc-set-spec ,name (gpc-util-alist-to-hash ,spec-list))))

(defalias 'gpc-make-local-variable 'nalist-make-local-variable)

(defalias 'gpc-make-variable-buffer-local 'nalist-make-variable-buffer-local)

(defmacro gpc-overwrite-with-initvals (cache)
  "Overwrite the whole cache content with initvals in the CACHE spec."
  `(maphash #'(lambda (k v)
                (nalist-set k (car v) ,cache))
            (gpc-get-spec ,cache)))

(cl-defmacro defcache (name buffer-local doc-string &rest spec-list)
  "Define NAME as a general purpose cache, and return the symbol.

This macro uses `defvar' internally so that the resulting symbol
as a variable is special, and DOC-STRING is stored in the
symbol's property list.

The resulting variable is initialized as an automatically
buffer-local variable if the value of BUFFER-LOCAL is
:buffer-local, otherwise, as a global variable.

SPEC-LIST defines the specification of the cache: its initial
values and fetch functions.  See `gpc-init' for the detail."
  (declare (indent 2))
  `(prog1
       (defvar ,name nil ,doc-string)
     (gpc-set-spec ,name (gpc-util-alist-to-hash ',spec-list))
     (nalist-init ,name nil)
     (when (eq ,buffer-local :buffer-local)
       (gpc-make-variable-buffer-local ,name))))

;; Cache spec access functions

(defmacro gpc-set-spec (symbol hash-table)
  "Set HASH-TABLE in SYMBOL's property list as its cache spec.

HASH-TABLE should contain a cache spec following the spec
description format.  See `gpc-init' for the detail."
  `(put ',symbol 'gpc-cache-spec ,hash-table))

(defmacro gpc-get-spec (cache)
  "Return the spec of CACHE."
  `(get ',cache 'gpc-cache-spec))

(defmacro gpc-spec-set-entry (key initval fetchfn cache)
  "Set the CACHE spec entry whose key is KEY to have the value (INITVAL FETCHFN)."
  `(puthash ,key (list ,initval ,fetchfn) (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-entry (key cache)
  "Return a CACHE spec entry with KEY if exists, otherwise nil.

A CACHE spec entry is a list: (KEY initval fetchfn)."
  `(gethash ,key (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-initval (key cache)
  "Get the initval of the CACHE spec entry with KEY."
  `(nth 0 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-get-fetchfn (key cache)
  "Get the fetch function of the CACHE spec entry with KEY."
  `(nth 1 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-map (function cache)
  "Call FUNCTION for all keys and values in CACHE.

The function should have three arguments, which are filled by
this macro with a key, its initval, and its fetchfn."
  `(maphash '(lambda (k v)
               (funcall ,function k (nth 0 v) (nth 1 v)))
            (gpc-get-spec ,cache)))

(defmacro gpc-spec-keyp (key cache)
  "Return t if KEY is a key in the CACHE's spec, otherwise nil."
  `(if (gpc-spec-get-entry ,key ,cache) t nil))

(defmacro gpc-pp-spec (cache)
  "Pretty print the CACHE spec, and return it."
  (let ((alist (cl-gensym "alist-")))
    `(let ((,alist (gpc-util-hash-to-alist
                    (gpc-get-spec ,cache))))
       (message (pp ,alist))
       ,alist)))

;; Cache content access functions

(defalias 'gpc-val 'nalist-get)

(defmacro gpc-fetch (key cache)
  "Fetch the value of KEY in CACHE by calling its fetch function.

It returns the fetched value."
  (let ((ekey (cl-gensym "key-")))
    `(let ((,ekey ,key))
       (if (gpc-locked-p ,cache)
           (gpc-val ,ekey ,cache)
         (nalist-set ,ekey (funcall (gpc-spec-get-fetchfn ,ekey ,cache)) ,cache)))))

(defmacro gpc-fetch-all (cache)
  "Fetch values for all keys in the CACHE spec."
  `(gpc-spec-map '(lambda (k v f)
                    (gpc-fetch k ,cache))
                 ,cache))

(cl-defmacro gpc-get (key cache &key (force nil))
  "Return the value of KEY in CACHE by calling the fetchfn if needed.

It uses fetchfn to get the value when FORCE is non-nil."
  (let ((ekey (cl-gensym "key-")))
    `(let ((,ekey ,key))
       (if ,force
           (gpc-fetch ,ekey ,cache)
         (if (gpc-pair-exist-p ,ekey ,cache)
             (gpc-val ,ekey ,cache)
           (gpc-fetch ,ekey ,cache))))))

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
  "Pretty print and return the whole content of CACHE."
  (message (pp cache))
  cache)

(defmacro gpc-pool-init (poolsymbol cache)
  "Initialize a gpc pool for CACHE with POOLSYMBOL.

A pool in `gpc' is a list of values stored in the POOLSYMBOL
property list."
  `(put ',cache ,poolsymbol nil))

(cl-defmacro gpc-pool-pushnew (value pool cache &key (test ''eql))
  "Put a VALUE into POOL of CACHE."
  (let ((pool-tmp (cl-gensym "pool-map-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (cl-pushnew ,value ,pool-tmp :test ,test)
       (put ',cache ,pool ,pool-tmp))))

(defmacro gpc-pool-clear (pool cache)
  "Clear all values in POOL of CACHE."
  `(put ',cache ,pool nil))

(defmacro gpc-pool-get-all (pool cache)
  "Get all values in POOL of CACHE."
  `(get ',cache ,pool))

(defmacro gpc-pool-set-all (value-list pool cache)
  "Replace the content of POOL of CACHE with VALUE-LIST.

This function doesn't copy VALUE-LIST.  If you ned to avoid
unintentional resouce sharing between cons cells, copy it
beforehand."
  `(put ',cache ,pool ,value-list))

(defmacro gpc-pool-map (function pool cache)
  "Call FUNCTION for all values in POOL of CACHE."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (mapc ,function ,pool-tmp))))

(cl-defmacro gpc-pool-member (value pool cache &key (test ''eql))
  "Find the first occurrence of VALUE in POOL of CACHE.
Return the sublist of POOL whose car is VALUE.

Keywords supported:  :test."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (cl-member ,value ,pool-tmp :test ,test))))

(defmacro gpc-pool-member-if (predicate pool cache)
  "Find the first item satisfying PREDICATE in POOL of CACHE.
Return the sublist of POOL whose car matches."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (cl-member-if ,predicate ,pool-tmp))))

(defmacro gpc-pool-member-if-not (predicate pool cache)
  "Find the first item satisfying PREDICATE in POOL of CACHE.
Return the sublist of POOL whose car matches."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (cl-member-if-not ,predicate ,pool-tmp))))

(cl-defmacro gpc-pool-delete (value pool cache &key (test ''eql))
  "Delete the all occurrences of VALUE in POOL of CACHE.

Keywords supported:  :test."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (put ',cache ,pool (cl-remove ,value ,pool-tmp :test ,test)))))

(defmacro gpc-pool-delete-if (predicate pool cache)
  "Delete all item satisfying PREDICATE in POOL of CACHE."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (put ',cache ,pool (cl-remove-if ,predicate ,pool-tmp)))))

(defmacro gpc-pool-delete-if-not (predicate pool cache)
  "Delete all item not satisfying PREDICATE in POOL of CACHE."
  (let ((pool-tmp (cl-gensym "pool-tmp-")))
    `(let ((,pool-tmp (get ',cache ,pool)))
       (put ',cache ,pool (cl-remove-if-not ,predicate ,pool-tmp)))))

(defmacro gpc-lock (cache)
  "Lock the values in CACHE, and return the lock list of CACHE.

After locking, `gpc-fetch' acts like `gpc-val'.  This gpc lock
feature is intended to be used with buffer-local variables.

The lock list of CACHE contains the buffers where CACHE is
locked."
  `(progn
     (gpc-lock-gc ,cache)
     (gpc-pool-pushnew (current-buffer) 'gpc-locked-buffers ,cache)))

(defmacro gpc-unlock (cache)
  "Unlock CACHE."
  `(gpc-pool-delete (current-buffer) 'gpc-locked-buffers ,cache))

(defmacro gpc-lock-clear (cache)
  "Set the lock list of CACHE nil."
  `(gpc-pool-clear 'gpc-locked-buffers ,cache))

(defmacro gpc-lock-gc (cache)
  "Remove killed buffers from the lock list of CACHE."
  `(gpc-pool-delete-if-not 'buffer-live-p 'gpc-locked-buffers ,cache))

(defmacro gpc-lock-pp (cache)
  "Pretty print the locked buffers for CACHE."
  `(message (pp (get ',cache 'gpc-locked-buffers))))

(defmacro gpc-lock-pp (cache)
  "Pretty print the locked buffers for CACHE."
  `(message (pp (gpc-pool-get-all 'gpc-locked-buffers ,cache))))

(defmacro gpc-get-lock-list (cache)
  "Return the lock list of CACHE."
  `(gpc-pool-get-all 'gpc-locked-buffers ,cache))

(defmacro gpc-locked-p (cache)
  "Return t if CACHE is locked, otherwise nil."
  `(if (member (current-buffer)
               (get ',cache 'gpc-locked-buffers))
       t nil))

(defmacro gpc-locked-p (cache)
  "Return t if CACHE is locked, otherwise nil."
  `(if (gpc-pool-member (current-buffer) 'gpc-locked-buffers ,cache)
       t nil))

(defmacro gpc-copy (cache from-buffer to-buffer)
  "Copy the content of CACHE from FROM-BUFFER to TO-BUFFER.

Use this function when CACHE is buffer-local or automatically
buffer-local."
  (let ((efrom-buffer (cl-gensym "from-buffer-"))
        (eto-buffer (cl-gensym "to-buffer-"))
        (content (cl-gensym "content-")))
    `(let ((,efrom-buffer ,from-buffer)
           (,eto-buffer ,to-buffer)
           (,content))
       (save-excursion
         (let ((content nil))
           (set-buffer ,efrom-buffer)
           (setq ,content (copy-alist ,cache))
           (set-buffer ,eto-buffer)
           (setq ,cache ,content))))))

(provide 'gpc)
;;; gpc.el ends here
