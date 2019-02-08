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

;;; Core functions.

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
     (gcache--copy-symbol-property 'gcache-cache-spec ',cache-spec ',symbol)
     (gcache--initialize-storage ,symbol)
     (gcache--set-default-content ,cache-spec ,symbol)
     (when ,buffer-local
       (make-variable-buffer-local ',symbol))))

(defmacro gcache--set-default-content (cache-spec cache)
  "Set the default content for CACHE from CACHE-SPEC."
  `(if (hash-table-p ,cache)
       ;; hash version
       (gcache--copy-keys-and-value-0 ,cache-spec ,cache)
     ;; alist version
     (setq ,cache (gcache--make-alist-from-key-and-value0 ,cache-spec))))

(defmacro gcache--initialize-storage (symbol)
  "Initializse a storage for a cache and bind it to SYMBOL."
  ;; hash version.
  ;;`(setq ,symbol (make-hash-table))
  ;; alist version.
  `(setq ,symbol nil))

(defun gcache-fetch (key cache)
  "Return the value of KEY in CACHE."
  (cdr (assoc key cache)))


(defun gcache-exist-p (entry cache)
  "Return t if CACHE has ENTRY, otherwise nil.

ENTRY should be a symbol."
  (if (assq entry cache) t nil))

(defmacro gcache-add (entry fetch-fun cache)
  "Add ENTRY to CACHE with FETCH-FUN, and return ENTRY."
  `(unless (gcache-exist-p ',entry ,cache )
     (setq ,cache
           (push (cons ',entry '(nil ,fetch-fun)) ,cache))
     ',entry))

(defun gcache-remove (entry cache)
  "Remove ENTRY from CACHE.

Return t if ENTRY exists, othewise nil."
  )


;;; Helper functions.

(defun gcache--copy-symbol-property (propname from-symbol to-symbol)
  "Copy a property named PROPNAME from FROM-SYMBOL to TO-SYMBOL."
  (put to-symbol propname
       (get from-symbol propname)))

(defun gcache--copy-keys-and-value-0 (from-cache to-cache)
  "Copy all keys from FROM-CACHE to TO-CACHE.

All of the values of the keys are set to 'no-value."
  (maphash #'(lambda (key value)
               (puthash key (nth 0 value) to-cache))
           from-cache))

(defun gcache--make-alist-from-key-and-value0 (hash)
  "Make and return an alist from keys and value0 of HASH."
  (let ((res nil))
    (maphash #'(lambda (key value)
                 (push (cons key (nth 0 value))
                       res))
             hash)
    res))

(defun gcache--dump (cache)
  "Dump CACHE in minibuffer."
  (message (pp cache)))

(provide 'gcache)
;;; gcache.el ends here
