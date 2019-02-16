;;; gpc-test.el --- Tests for gpc

(require 'ert)
(require 'f)
(require 's)

(load (f-expand "gpc.el" default-directory))


;;; Setup functions.

(defun a-retriever ()
  t)

(defun setup-gpc-defcache ()
  (gpc-defcache g-cache 'global
    "a global cache."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever))

  (gpc-defcache l-cache 'buffer-local
    "a buffer-local cache."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever)))

;;; Tests.

(ert-deftest test-namespace-polution ()
  (should (if gpc-namespace-polution
              (eq (fboundp 'defcache) t)
            (eq (fboundp 'defacache) nil))))

(ert-deftest test-gpc-spec-get ()
  (setup-gpc-defcache)
  (should (equal (gpc-spec-get-entry 'true g-cache)
                 '(nil a-retriever))))

(ert-deftest test-gpc-spec-set-engry ()
  (setup-gpc-defcache)
  (should (equal (gpc-spec-get-entry 'true g-cache)
                 '(nil a-retriever)))
  (gpc-spec-set-entry 'true t 'a-fetchfn g-cache)
  (should (equal (gpc-spec-get-entry 'true g-cache)
                 '(t a-fetchfn)))
  (gpc-spec-set-entry 'test "testing" 'a-fetchfn g-cache)
  (should (equal (gpc-spec-get-entry 'test g-cache)
                 '("testing" a-fetchfn)))
  )

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

(ert-deftest test-gpc-keyp ()
  (setup-gpc-defcache)
  (gpc-copy-init-values g-cache)
  (should (eq (gpc-keyp 'spam g-cache) nil))
  (should (eq (gpc-keyp 'pwd g-cache) t)))

(ert-deftest test-gpc-fetch ()
  (setup-gpc-defcache)
  (gpc-copy-init-values g-cache)
  (should (equal (gpc-fetch 'pwd g-cache) "/"))
  (gpc-clear g-cache)
  (should (equal (s-chop-suffix "/" (gpc-fetch 'pwd g-cache))
                 (s-chop-suffix "/" (f-expand default-directory)))))

(ert-deftest test-gpc-defcache ()
  (gpc-defcache acache 'global
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
  ;;(should (equal (gpc-fetch 'pwd acache) "/"))
  )

(ert-deftest test-gpc-clear ()
  (setup-gpc-defcache)
  (gpc-clear g-cache)
  (should (eq g-cache nil)))

(ert-deftest test-gpc-spec-get-fetchfn ()
  (setup-gpc-defcache)
  (should (eq (gpc-spec-get-fetchfn
               'true
               g-cache)
              'a-retriever)))

(ert-deftest test-gpc-get-spec ()
  (setup-gpc-defcache)
  (should (equal (gethash 'true (gpc-get-spec g-cache))
                 '(nil a-retriever))))

(ert-deftest test-gpc-set-default-content ()
  (setup-gpc-defcache)
  (gpc-copy-init-values g-cache)
  (should (equal (gpc-fetch 'pwd g-cache)
                 "/")))

(ert-deftest test-gpc-remove ()
  (setup-gpc-defcache)
  (gpc-remove 'pwd g-cache)
  (should (eq (gpc-keyp 'pwd g-cache)
              nil)))

;;; Tests for helper functions.

(ert-deftest test-gpc-util-make-alist-from-key-and-value0 ()
  (setq hash-a (make-hash-table))
  (puthash 'a '(1 10) hash-a)
  (puthash 'b '(2 20) hash-a)
  (should (nalist-set-equal-p
           (gpc-util-make-alist-from-key-and-value0 hash-a)
           '((a . 1) (b . 2))))
  )

;;; Tests for test helper functions.

(ert-deftest test-is-subset-hash-of ()
  (setq hash-a (make-hash-table))
  (setq hash-b (make-hash-table))
  (setq hash-c (make-hash-table))
  (puthash 'a 1 hash-a)
  (puthash 'a 1 hash-b)
  (puthash 'b 2 hash-b)
  (puthash 'c 3 hash-c)
  (should (eq (is-subset-hash-of (make-hash-table) (make-hash-table))
              t))
  (should (eq (is-subset-hash-of hash-a hash-b)
              t))
  (should (eq (is-subset-hash-of hash-b hash-a)
              nil))
  (should (eq (is-subset-hash-of hash-a hash-c)
              nil))
  )

(ert-deftest test-hash-equal ()
  (setq hash-a (make-hash-table))
  (setq hash-b (make-hash-table))
  (setq hash-c (make-hash-table))
  (setq hash-d (make-hash-table))
  (puthash 'a 1 hash-a)
  (puthash 'a 1 hash-b)
  (puthash 'b 2 hash-b)
  (puthash 'c 3 hash-c)
  (puthash 'a 1 hash-d)
  (puthash 'b 2 hash-d)
  (should (eq (hash-equal (make-hash-table) (make-hash-table))
              t))
  (should (eq (hash-equal hash-a hash-b)
              nil))
  (should (eq (hash-equal hash-b hash-a)
              nil))
  (should (eq (hash-equal hash-a hash-c)
              nil))
  (should (eq (hash-equal hash-b hash-d)
              t))
  )



;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; gpc-test.el ends here
