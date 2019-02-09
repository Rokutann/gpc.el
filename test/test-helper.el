;;; test-helper.el --- Helpers for elcache-test.el

(defun is-subset-hash-of (hash-a hash-b)
  "Test if HASH-A is a subset of HASH-B."
  (let ((res t))
    (maphash #'(lambda (key value)
                 (unless (equal (gethash key hash-b)
                                value)
                   (setq res nil)))
             hash-a)
    res))


(defun hash-equal (hash-a hash-b)
  "Test is HASH-A and HASH-B are equal."
  (and (is-subset-hash-of hash-a hash-b)
       (is-subset-hash-of hash-b hash-a)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; test-helper.el ends here
