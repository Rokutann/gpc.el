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

(defun gpc-helper-seq-set-equal-p (seq-a seq-b)
  (if (and (sequencep seq-a) (sequencep seq-b))
      (gpc-helper-seq-set-equal-p-recur seq-a seq-b)
    nil))

(defun gpc-helper-seq-set-equal-p-recur (seq-a seq-b)
  (if (not (= (length seq-a) (length seq-b)))
      nil
    (cond ((and (= (length seq-a) 0) (= (length seq-b) 0)) t)
          ((= (length seq-a) 0) nil)
          ((= (length seq-b) 0) nil)
          (t (if (seq-contains seq-b (seq-elt seq-a 0))
                 (gpc-helper-seq-set-equal-p-recur (seq-subseq seq-a 1) (remove (seq-elt seq-a 0) seq-b))
               nil)))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; test-helper.el ends here
