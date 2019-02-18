;;; gpc-test.el --- Tests for gpc

(require 'ert)
(require 'f)
(require 's)
(require 'nalist)

(load (f-expand "gpc.el" default-directory))


;;; Setup functions.

(defun a-retriever ()
  t)

(defun setup-gpc-defcache ()
  (gpc-defgpc g-cache 'global
    "a global cache."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever)
    (uname "generic" (lambda ()
                       (with-temp-buffer
                         (call-process "uname" nil t)
                         (s-chop-suffix "\n" (buffer-string))))))

  (gpc-defgpc l-cache 'buffer-local
    "a buffer-local cache."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever)))

;;; Tests.

(ert-deftest gpc-set-spec-test/hash-table-generator ()
  (unintern "gpc-var")
  (gpc-set-spec gpc-var (make-hash-table))
  (should (hash-table-p (get 'gpc-var 'gpc-cache-spec))))

(ert-deftest gpc-set-spec-test/hash-table-variable ()
  (unintern "gpc-var")
  (setq ht (make-hash-table))
  (gpc-set-spec gpc-var ht)
  (should (hash-table-p (get 'gpc-var 'gpc-cache-spec))))

(ert-deftest gpc-get-spec-test ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (put 'gpc-var 'gpc-cache-spec (make-hash-table))
  (should (hash-table-p (gpc-get-spec gpc-var))))

(ert-deftest gpc-spec-set-entry-test ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (gpc-spec-set-entry 'a 'b '(lambda () 'c) gpc-var)
  (setq spec (gpc-get-spec gpc-var))
  (should (equal (gethash 'a spec) '(b (lambda () 'c)))))

(ert-deftest gpc-spec-get-entry-test ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (gpc-spec-set-entry 'a 'b '(lambda () 'c) gpc-var)
  (should (equal (gpc-spec-get-entry 'a gpc-var) '(b (lambda () 'c)))))

(ert-deftest gpc-spec-get-initval-test ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (gpc-spec-set-entry 'a 'b '(lambda () 'c) gpc-var)
  (should (eq (gpc-spec-get-initval 'a gpc-var) 'b)))

(ert-deftest gpc-spec-get-fetchfn-test ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (gpc-spec-set-entry 'a 'b '(lambda () 'c) gpc-var)
  (should (equal (gpc-spec-get-fetchfn 'a gpc-var) '(lambda () 'c))))

(ert-deftest gpc-spec-keyp-test/nil ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (should-not (gpc-spec-keyp 'a gpc-var)))

(ert-deftest gpc-spec-keyp-test/existent-key ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (gpc-spec-set-entry 'a 'b '(lambda () 'c) gpc-var)
  (should (gpc-spec-keyp 'a gpc-var)))

(ert-deftest gpc-spec-keyp-test/non-existent-key ()
  (unintern "gpc-var")
  (setq gpc-var nil)
  (gpc-set-spec gpc-var (make-hash-table))
  (gpc-spec-set-entry 'a 'b '(lambda () 'c) gpc-var)
  (should-not (gpc-spec-keyp 'b gpc-var)))

(ert-deftest gpc-util-hash-to-alist/hash-table-with-no-entry ()
  (setq ht (make-hash-table))
  (should (eq (gpc-util-hash-to-alist ht) nil)))

(ert-deftest gpc-util-hash-to-alist/hash-table-with-one-entry ()
  (setq ht (make-hash-table))
  (puthash 'a 'b ht)
  (should (equal (gpc-util-hash-to-alist ht) '((a . b)))))

(ert-deftest gpc-util-hash-to-alist/hash-table-with-two-entries ()
  (setq ht (make-hash-table))
  (puthash 'a 'b ht)
  (puthash 'c 'd ht)
  (should (nalist-set-equal-p (gpc-util-hash-to-alist ht) '((a . b) (c . d)))))

(ert-deftest gpc-util-alist-to-hash/alist-is-nil ()
  (setq alist nil)
  (should (= (hash-table-count (gpc-util-alist-to-hash alist)) 0)))

(ert-deftest gpc-util-alist-to-hash/alist-with-one-pair ()
  (setq alist '((a . b)))
  (setq ht (gpc-util-alist-to-hash alist))
  (should (= (hash-table-count ht) 1))
  (should (eq (gethash 'a ht) 'b)))

(ert-deftest gpc-util-alist-to-hash/alist-with-two-pairs ()
  (setq alist '((a . b) (c . d)))
  (setq ht (gpc-util-alist-to-hash alist))
  (should (= (hash-table-count ht) 2))
  (should (eq (gethash 'a ht) 'b))
  (should (eq (gethash 'c ht) 'd)))

(ert-deftest gpc-init-test/spec-with-no-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var nil)
  (should (eq gpc-var nil))
  (should (eq (hash-table-count (gpc-get-spec gpc-var)) 0)))

(ert-deftest gpc-init-test/spec-with-one-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((a b (lambda () nil))))
  (should (eq gpc-var nil))
  (should (eq (gpc-spec-get-initval 'a gpc-var) 'b))
  (should (equal (gpc-spec-get-fetchfn 'a gpc-var) '(lambda () nil))))

(ert-deftest gpc-init-test/spec-with-two-entries ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((a b (lambda () nil))
                      (c d (lambda () t))))
  (should (eq gpc-var nil))
  (should (eq (gpc-spec-get-initval 'a gpc-var) 'b))
  (should (eq (gpc-spec-get-initval 'c gpc-var) 'd))
  (should (equal (gpc-spec-get-fetchfn 'a gpc-var) '(lambda () nil)))
  (should (equal (gpc-spec-get-fetchfn 'c gpc-var) '(lambda () t))))

(ert-deftest gpc-overwrite-with-initvals-test/spec-with-no-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var nil)
  (gpc-overwrite-with-initvals gpc-var)
  (should (eq gpc-var nil)))

(ert-deftest gpc-overwrite-with-initvals-test/spec-with-one-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((a b (lambda () nil))))
  (gpc-overwrite-with-initvals gpc-var)
  (should (= (length gpc-var) 1))
  (should (eq (gpc-val 'a gpc-var) 'b)))

(ert-deftest gpc-overwrite-with-initvals-test/spec-with-two-entries ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((a b (lambda () nil))
                      (c d (lambda () t))))
  (gpc-overwrite-with-initvals gpc-var)
  (should (= (length gpc-var) 2))
  (should (eq (gpc-val 'a gpc-var) 'b))
  (should (eq (gpc-val 'c gpc-var) 'd)))

(ert-deftest gpc-fetch-test ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((uname "Hurd" (lambda ()
                                      (with-temp-buffer
                                        (call-process "uname" nil t)
                                        (s-chop-suffix "\n" (buffer-string)))))))
  (should (equal (gpc-fetch 'uname gpc-var) "Darwin")))

(ert-deftest gpc-get-test ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((uname "Hurd" (lambda ()
                                      (with-temp-buffer
                                        (call-process "uname" nil t)
                                        (s-chop-suffix "\n" (buffer-string)))))))
  (gpc-overwrite-with-initvals gpc-var)
  (should (equal (gpc-get 'uname gpc-var) "Hurd"))
  (gpc-clear gpc-var)
  (should (equal (gpc-get 'uname gpc-var) "Darwin")))

(ert-deftest gpc-fetch-all-test/no-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var nil)
  (gpc-fetch-all gpc-var)
  (should (eq gpc-var nil)))

(ert-deftest gpc-fetch-all-test/one-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((system "Hurd" (lambda ()
                                       (with-temp-buffer
                                         (call-process "uname" nil t)
                                         (s-chop-suffix "\n" (buffer-string)))))))
  (gpc-fetch-all gpc-var)
  (should (equal (gpc-fetch 'system gpc-var) "Darwin")))

(ert-deftest gpc-fetch-all-test/two-entries ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((system "Hurd" (lambda ()
                                       (with-temp-buffer
                                         (call-process "uname" nil t)
                                         (s-chop-suffix "\n" (buffer-string)))))
                      (machine "mips" (lambda ()
                                        (with-temp-buffer
                                          (call-process "uname" nil t nil "-m")
                                          (s-chop-suffix "\n" (buffer-string)))))))
  (gpc-fetch-all gpc-var)
  (should (equal (gpc-fetch 'system gpc-var) "Darwin"))
  (should (equal (gpc-fetch 'machine gpc-var) "x86_64")))

;; FIXME: This test of the feature should be more testing friendly.
(ert-deftest namespace-pollution-test ()
  (should (if gpc-namespace-pollution
              (eq (fboundp 'defgpc) t)
            (eq (fboundp 'defgpc) nil))))

(ert-deftest gpc-map-test/spec-is-nil ()
  (unintern "gpc-var")
  (gpc-init gpc-var nil)
  (let ((res nil))
    (gpc-spec-map '(lambda (k v f) (push (list k v f) res)) gpc-var)
    (should (eq res nil))))

(ert-deftest gpc-map-test/spec-has-one-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((a b (lambda () nil))))
  (let ((res nil))
    (gpc-spec-map '(lambda (k v f) (push (list k v f) res)) gpc-var)
    (should (equal res '((a b (lambda () nil)))))))

(ert-deftest gpc-map-test/spec-has-one-entry ()
  (unintern "gpc-var")
  (gpc-init gpc-var '((a b (lambda () nil))
                      (c d (lambda () t))))
  (let ((res nil))
    (gpc-spec-map '(lambda (k v f) (push (list k v f) res)) gpc-var)
    (should (nalist-set-equal-p res '((a b (lambda () nil))
                                      (c d (lambda () t)))))))


;;; Refactored downto here.


(ert-deftest test-gpc-defcache-buffer-local ()
  (setup-gpc-defcache)
  (should (eq (local-variable-if-set-p 'g-cache) nil))
  (should (eq (local-variable-if-set-p 'l-cache) t)))

(ert-deftest test-gpc-defcache-docstring ()
  (setup-gpc-defcache)
  (should (equal (documentation-property 'g-cache 'variable-documentation)
                 "a global cache."))
  (should (equal (documentation-property 'l-cache 'variable-documentation)
                 "a buffer-local cache.")))

(ert-deftest test-gpc-defcache ()
  (gpc-defgpc acache 'global
    ""
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever)                     )
  ;; (should (eq (hash-table-p acache)
  ;;             t))
  (should (eq (hash-table-p (get 'acache 'gpc-cache-spec))
              t))
  ;; (should (equal (gethash 'pwd acache)
  ;;                "/"))
  ;; (should (equal (gethash 'pwd acache)
  ;;                "/"))
  ;;(should (equal (gpc-get 'pwd acache) "/"))
  )



;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; gpc-test.el ends here
