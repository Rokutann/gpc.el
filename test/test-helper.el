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

(defun is-subset-alist-of (alist-a alist-b)
  "Return t is ALIST-A is a sbuset of ALIST-B, otherwise nil."
  (let ((res t))
    (mapcar #'(lambda (pair)
                (unless (member pair alist-b)
                  (setq res nil)))
            alist-a)
    res))

(defun alist-equal (alist-a alist-b)
  (and (is-subset-alist-of alist-a alist-b)
       (is-subset-alist-of alist-b alist-a)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; test-helper.el ends here
