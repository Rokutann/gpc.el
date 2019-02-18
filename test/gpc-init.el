;;; gpc-init.el --- Nalist: Initialization for the tests.  -*- lexical-binding: t; -*-
;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: Lisp, tools
;; URL: https://github.com/mukuge/gpc.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'seq)
(require 'ert)
(require 'f)
(require 's)
(require 'nalist)

(defvar gpc-test/test-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to tests directory.")

(defvar gpc-test/root-path
  (directory-file-name (file-name-directory gpc-test/test-path))
  "Path to root directory.")

(load (expand-file-name "gpc" gpc-test/root-path) 'noerror 'nomessage)

(provide 'gpc-init)
;;; gpc-init.el ends here
