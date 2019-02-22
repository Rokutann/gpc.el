;;; gpc-integration-test.el --- gpc: Integration tests.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: lisp

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

;; Integration tests for gpc.

;;; Code:

(ert-deftest gpc-integration-test/buffer-local ()
  (unintern "gpcit-2" nil)
  (setq testing-buffer-name (buffer-name))
  (setq system-value (s-chop-suffix "\n" (shell-command-to-string "uname")))
  (setq machine-value (s-chop-suffix "\n" (shell-command-to-string "uname -m")))
  (gpc-init gpcit-2 '((system "Hurd" (lambda ()
                                       (s-chop-suffix "\n" (shell-command-to-string "uname"))))
                      (machine "mips" (lambda ()
                                        (s-chop-suffix "\n" (shell-command-to-string "uname -m"))))))
  (should (eq gpcit-2 nil))
  (should-not (local-variable-p 'gpcit-2))
  (should-not (local-variable-if-set-p 'gpcit-2))
  (gpc-overwrite-with-initvals gpcit-2)
  (should (equal (gpc-val 'system gpcit-2) "Hurd"))
  (with-temp-buffers (buffer-a buffer-b)
                     (set-buffer buffer-a)
                     (should (equal (gpc-val 'system gpcit-2) "Hurd"))
                     (should (equal (gpc-fetch 'system gpcit-2) system-value))
                     (set-buffer testing-buffer-name)
                     (should (equal (gpc-fetch 'system gpcit-2) system-value))
                     (set-buffer buffer-b)
                     (should (equal (gpc-fetch 'system gpcit-2) system-value))
                     (gpc-make-local-variable gpcit-2)
                     (should (equal (gpc-fetch 'system gpcit-2) system-value))
                     (should (equal (gpc-val 'machine gpcit-2) "mips"))
                     (should (equal (gpc-fetch 'machine gpcit-2) machine-value))
                     (set-buffer buffer-a)
                     (should (equal (gpc-val 'machine gpcit-2) "mips"))
                     (gpc-set 'machine "z80" gpcit-2)
                     (should (equal (gpc-val 'machine gpcit-2) "z80"))
                     (set-buffer buffer-b)
                     (should (equal (gpc-fetch 'machine gpcit-2) machine-value))
                     (set-buffer testing-buffer-name)
                     (should (equal (gpc-val 'machine gpcit-2) "z80"))
                     (with-temp-buffers (buffer-c)
                                        (set-buffer buffer-c)
                                        (should (equal (gpc-val 'machine gpcit-2) "z80")))))

(ert-deftest gpc-integration-test/automatically-buffer-local/buffers-created-after-going-local ()
  (unintern "gpcit-1" nil)
  (setq testing-buffer-name (buffer-name))
  (setq system-value (s-chop-suffix "\n" (shell-command-to-string "uname")))
  (setq machine-value (s-chop-suffix "\n" (shell-command-to-string "uname -m")))
  (gpc-init gpcit-1 '((system "Hurd" (lambda ()
                                       (s-chop-suffix "\n" (shell-command-to-string "uname"))))
                      (machine "mips" (lambda ()
                                        (s-chop-suffix "\n" (shell-command-to-string "uname -m"))))))
  (should (eq gpcit-1 nil))
  (should-not (local-variable-p 'gpcit-1))
  (should-not (local-variable-if-set-p 'gpcit-1))
  (gpc-make-variable-buffer-local gpcit-1)
  (should (local-variable-p 'gpcit-1))
  (should (local-variable-if-set-p 'gpcit-1))
  (gpc-overwrite-with-initvals gpcit-1)
  (should (equal (gpc-val 'system gpcit-1) "Hurd"))
  (with-temp-buffers (buffer-a buffer-b)
                     (set-buffer buffer-a)
                     (should (eq gpcit-1 nil))
                     (should (equal (gpc-fetch 'system gpcit-1) system-value))
                     (should (equal (gpc-val 'system gpcit-1) system-value))
                     (set-buffer testing-buffer-name)
                     (should (equal (gpc-val 'system gpcit-1) "Hurd"))
                     (set-buffer buffer-b)
                     (should (eq (gpc-val 'system gpcit-1) nil))
                     (should (equal (gpc-get 'machine gpcit-1) machine-value))
                     (set-buffer buffer-a)
                     (should (eq (gpc-val 'machine gpcit-1) nil))
                     (set-buffer testing-buffer-name)
                     (should (equal (gpc-val 'machine gpcit-1) "mips"))))

(ert-deftest gpc-integration-test/automatically-buffer-local/buffers-exist-before-going-local ()
  (unintern "gpcit-3" nil)
  (setq testing-buffer-name (buffer-name))
  (setq system-value (s-chop-suffix "\n" (shell-command-to-string "uname")))
  (setq machine-value (s-chop-suffix "\n" (shell-command-to-string "uname -m")))
  (gpc-init gpcit-3 '((system "Hurd" (lambda ()
                                       (s-chop-suffix "\n" (shell-command-to-string "uname"))))
                      (machine "mips" (lambda ()
                                        (s-chop-suffix "\n" (shell-command-to-string "uname -m"))))))
  (should (eq gpcit-3 nil))
  (should-not (local-variable-p 'gpcit-3))
  (should-not (local-variable-if-set-p 'gpcit-3))
  (with-temp-buffers (buffer-a buffer-b)
                     (gpc-make-variable-buffer-local gpcit-3)
                     (should (local-variable-p 'gpcit-3))
                     (should (local-variable-if-set-p 'gpcit-3))
                     (gpc-overwrite-with-initvals gpcit-3)
                     (should (equal (gpc-val 'system gpcit-3) "Hurd"))
                     (set-buffer buffer-a)
                     (should (eq gpcit-3 nil))
                     (should (equal (gpc-fetch 'system gpcit-3) system-value))
                     (should (equal (gpc-val 'system gpcit-3) system-value))
                     (set-buffer testing-buffer-name)
                     (should (equal (gpc-val 'system gpcit-3) "Hurd"))
                     (set-buffer buffer-b)
                     (should (eq (gpc-val 'system gpcit-3) nil))
                     (should (equal (gpc-get 'machine gpcit-3) machine-value))
                     (set-buffer buffer-a)
                     (should (eq (gpc-val 'machine gpcit-3) nil))
                     (set-buffer testing-buffer-name)
                     (should (equal (gpc-val 'machine gpcit-3) "mips"))))

(provide 'gpc-integration-test)
;;; gpc-integration-test.el ends here
