;;; gpc.el --- A general purpose cache fascility.   -*- lexical-binding: t; -*-

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
(require 'nalist)

;; User facing variables.

(defvar gpc-namespace-polution nil
  "Allow to polute the namespace if t, otherwise don't.

In the current version, `defcache' is the only polution this
option introduces.")

;; Util functions.

(defun gpc-util-hash-to-alist (hash)
  "Return an alist made from keys and values of HASH."
  (let ((alist nil))
    (maphash #'(lambda (k v) (nalist-set k v alist)) hash)
    alist))

(defun gpc-util-alist-to-hash (alist)
  "Return a hash tablen made from keys and values of ALIST."
  (let ((hash (make-hash-table)))
    (nalist-map #'(lambda (k v) (puthash k v hash)) alist)
    hash))

;; Core functions.

(cl-defmacro gpc-defcache (symbol buffer-local doc-string &rest body)
  "Define SYMBOL as a general purpose cache or gpc, and return SYMBOL.

A general purpose cache, or gpc, uses two places to store its
information: One is its value which is an alist, the other is in
its symbol-plist with the key `gpc-get-spec', whose associated
value is a hash-table.

The hash table contains the specification of the cache. Its key
is the cahche key, and the key's value is a list of its
default value and fetch function.

The cache is initialized as an automatically buffer-local
variable if the value of BUFFER-LOCAL is
'buffer-local. Otherwise, as a global variable defined by
`defvar'.

The BODY defines the spec of the cache. Each line of it is a list
containing a key, an initial value, and a fetch function in this
order.

Here is a call example:
\(gpc-defcache g-cache 'global
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
     (gpc-set-spec ,symbol (gpc-util-alist-to-hash ',body))
     (nalist-init ,symbol nil)
     (when (eq ,buffer-local 'buffer-local)
       (nalist-make-variable-buffer-local ,symbol))))

(when gpc-namespace-polution
  (defalias 'defcache 'gpc-defcache))

(defmacro gpc-copy-init-values (cache)
  "Copy the init values from CACHE's spec to CACHE."
  `(maphash #'(lambda (k v)
                (nalist-set k (car v) ,cache))
            (gpc-get-spec ,cache)))

(defalias 'gpc-val 'nalist-get
  "Return the value of KEY in CACHE if exists, otherwise nil.

This is an alias of `nalist-get'.")

(defmacro gpc-fetch (key cache)
  "Fetch the value of KEY in CACHE with its fetch function.

It reurns the value associated to KEY."
  `(nalist-set ,key (funcall (gpc-spec-get-fetchfn ,key ,cache)) ,cache))

(defalias 'gpc-set 'nalist-set
  "Set the value of KEY in CACHE (NALIST) to VALUE.

Add a new pair to CACHE if KEY doesn't exist in it.

This is an alias of `nalist-set'.")

(cl-defmacro gpc-get (key cache &key (force nil))
  "Return the value of KEY in CACHE by calling the fetchfn if needed."
  `(if ,force
       (gpc-fetch ,key ,cache)
     (if (gpc-pairp ,key ,cache)
         (gpc-val ,key ,cache)
       (gpc-fetch ,key ,cache))))

(cl-defmacro gpc-remove (key cache &key (testfn ''eq))
  "Remove the entry with KEY from CACHE."
  `(nalist-remove ,key ,cache :testfn ,testfn))

(defalias 'gpc-pairs 'nalist-pairs
  "Return a list consisting all the pairs in NALIST (CACHE).")

(defalias 'gpc-keys 'nalist-keys
  "Return a list consisting all the keys in NALIST (CACHE).")

(defalias 'gpc-values 'nalist-values
  "Return a list consisting all the values in NALIST (CACHE).")

(defmacro gpc-clear (cache)
  "Clear all keys and values in CACHE."
  `(nalist-clear ,cache))

(cl-defmacro gpc-keyp (key cache)
  "Return t if KEY defined in CACHE spec, otherwise nil."
  `(gpc-spec-keyp ,key ,cache))

(cl-defun gpc-pairp (key cache &key (testfn 'eq))
  "Return t if CACHE has an entry with KEY, otherwise nil."
  (nalist-get key cache :default nil :testfn testfn))

(defun gpc-pp (cache)
  "Show the content of CACHE and return it."
  (message (pp cache))
  cache)

;; Spec functions

(defmacro gpc-set-spec (cache spec-ht)
  "Set SPEC-HT as the CACHE's spec."
  `(put ',cache 'gpc-cache-spec ,spec-ht))

(defmacro gpc-get-spec (cache)
  "Return the spec of CACHE.

A cache spec is a hash table."
  `(get ',cache 'gpc-cache-spec))

(defmacro gpc-pp-spec (cache)
  "Pp the spec of CACHE, and return it."
  (let ((alist (gensym)))
    `(let ((,alist (gpc-util-hash-to-alist
                    (gpc-get-spec ,cache))))
       (message (pp ,alist))
       ,alist)))

(defmacro gpc-spec-set-entry (key initval fetchfn cache)
  "Set the CACHE's spec entry wchic contain  KEY, INITVAL, and FETCHFN."
  `(puthash ,key (list ,initval ,fetchfn) (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-entry (key cache)
  "Return a entry with KEY if it's in CACHE's spec, otherwise nil."
  `(gethash ,key (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-initval (key cache)
  "Get the initial value function for KEY in CACHE's spec."
  `(nth 0 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-get-fetchfn (key cache)
  "Get the fetch function for KEY in CACHE's spec."
  `(nth 1 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-keyp (key cache)
  "Return t if KEY is a key in CACHE's spec, othersise nil."
  `(if (gpc-spec-get-entry ,key ,cache) t nil))

(provide 'gpc)
;;; gpc.el ends here
