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

(defmacro gcache-defcache-spec (symbol doc-string spec)
  "Define a cache SPEC with DOC-STRING, and return SYMBOL."
  `(prog1
       (defvar ,symbol nil ,doc-string)
     (put ',symbol 'gcache-cache-spec ',symbol)
     (setq ,symbol (make-hash-table))
     (mapcar #'(lambda (entry)
                 (puthash (car entry) (cdr entry) ,symbol))
             ,spec)))

(defmacro gcache-cache-spec-p (symbol)
  "Return t if SYMBOL is a cache spec, otherwise nil."
  `(if (get ',symbol 'gcache-cache-spec)
       t
     nil))

(cl-defmacro defcache (symbol &key buffer-local doc-string)
  "Define SYMBOL as a cache, and return SYMBOL.

The new cache is automatically-buffer-local when BUFFER-LOCAL is
non-nil, otherwise global.

A cache is an alist with this structure:
(('a-cache-entry value . fetch-fucntion)"
  `(prog1
       (defvar ,symbol nil ,doc-string)
     (when ,buffer-local
       (make-variable-buffer-local ',symbol))))

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

(defun gcache--dump (cache)
  "Dump CACHE in minibuffer."
  (message (pp cache)))

(provide 'gcache)
;;; gcache.el ends here
