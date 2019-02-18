;;; test-helper.el --- Helpers for elcache-test.el

(defun gpc-helper-compose-generate-buffer-forms (sym-list)
  "Compose forms from SYM-LIST for `with-temp-buffers'."
  (mapcar #'(lambda (sym)
              `(setq ,sym (generate-new-buffer "temp")))
          sym-list))

(defun gpc-helper-compose-kill-buffer-forms (sym-list)
  "Compose forms from SYM-LIST for `with-temp-buffers'."
  (mapcar #'(lambda (sym)
              `(kill-buffer ,sym))
          sym-list))

(defmacro with-temp-buffers (sym-list &rest body)
  "Generate new buffers, bind them to symbols in SYM-LIST, and do BODY."
  (declare (indent 1))
  `(unwind-protect
       (progn
         ,@(gpc-helper-compose-generate-buffer-forms sym-list)
         ,@body)
     ,@(gpc-helper-compose-kill-buffer-forms sym-list)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; test-helper.el ends here
