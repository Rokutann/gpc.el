;;; gcache-test.el --- Tests for gcache

(require 'ert)
(require 'f)
(require 's)

(load (f-expand "gcache.el" default-directory))


;;; Setup functions.

(defun a-retriever ()
  t)

(defun setup-gcache-defcache ()
  (gcache-defcache g-cache 'global
    "a global cache."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever))

  (gcache-defcache l-cache 'buffer-local
    "a buffer-local cache."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever)))

;;; Tests.

(ert-deftest test-namespace-polution ()
  (should (if gcache-namespace-polution
              (eq (fboundp 'defcache) t)
            (eq (fboundp 'defacache) nil))))

(ert-deftest test-gcache-spec-get ()
  (setup-gcache-defcache)
  (should (equal (gcache-spec-get-entry 'true g-cache)
                 '(nil a-retriever))))

(ert-deftest test-gcache-spec-set-engry ()
  (setup-gcache-defcache)
  (should (equal (gcache-spec-get-entry 'true g-cache)
                 '(nil a-retriever)))
  (gcache-spec-set-entry 'true t 'a-fetchfn g-cache)
  (should (equal (gcache-spec-get-entry 'true g-cache)
                 '(t a-fetchfn)))
  (gcache-spec-set-entry 'test "testing" 'a-fetchfn g-cache)
  (should (equal (gcache-spec-get-entry 'test g-cache)
                 '("testing" a-fetchfn)))
  )

(ert-deftest test-gcache-defcache-buffer-local ()
  (setup-gcache-defcache)
  (should (eq (local-variable-if-set-p 'g-cache) nil))
  (should (eq (local-variable-if-set-p 'l-cache) t)))

(ert-deftest test-gcache-defcache-docstring ()
  (setup-gcache-defcache)
  (should (equal (documentation-property 'g-cache 'variable-documentation)
                 "a global cache."))
  (should (equal (documentation-property 'l-cache 'variable-documentation)
                 "a buffer-local cache.")))

(ert-deftest test-gcache-keyp ()
  (setup-gcache-defcache)
  (gcache-set-default-content g-cache)
  (should (eq (gcache-keyp 'spam g-cache) nil))
  (should (eq (gcache-keyp 'pwd g-cache) t)))

(ert-deftest test-gcache-fetch ()
  (setup-gcache-defcache)
  (gcache-set-default-content g-cache)
  (should (equal (gcache-fetch 'pwd g-cache) "/"))
  (gcache-clear g-cache)
  (should (equal (s-chop-suffix "/" (gcache-fetch 'pwd g-cache))
                 (s-chop-suffix "/" (f-expand default-directory)))))

(ert-deftest test-gcache-defcache ()
  (gcache-defcache acache 'global
    ""
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever)                     )
  ;; (should (eq (hash-table-p acache)
  ;;             t))
  (should (eq (hash-table-p (get 'acache 'gcache-cache-spec))
              t))
  ;; (should (equal (gethash 'pwd acache)
  ;;                "/"))
  ;; (should (equal (gethash 'pwd acache)
  ;;                "/"))
  ;;(should (equal (gcache-fetch 'pwd acache) "/"))
  )

(ert-deftest test-gcache-clear ()
  (setup-gcache-defcache)
  (gcache-clear g-cache)
  (should (eq g-cache nil)))

(ert-deftest test-gcache-spec-get-fetchfn ()
  (setup-gcache-defcache)
  (should (eq (gcache-spec-get-fetchfn
               'true
               g-cache)
              'a-retriever)))

(ert-deftest test-gcache-get-spec ()
  (setup-gcache-defcache)
  (should (equal (gethash 'true (gcache-get-spec g-cache))
                 '(nil a-retriever))))

(ert-deftest test-gcache-set-default-content ()
  (setup-gcache-defcache)
  (gcache-set-default-content g-cache)
  (should (equal (gcache-fetch 'pwd g-cache)
                 "/")))

(ert-deftest test-gcache-remove ()
  (setup-gcache-defcache)
  (gcache-remove 'pwd g-cache)
  (should (eq (gcache-keyp 'pwd g-cache)
              nil)))

;;; Tests for helper functions.

(ert-deftest test-gcache-util-make-alist-from-key-and-value0 ()
  (setq hash-a (make-hash-table))
  (puthash 'a '(1 10) hash-a)
  (puthash 'b '(2 20) hash-a)
  (should (nalist-set-equal-p
           (gcache-util-make-alist-from-key-and-value0 hash-a)
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

;;; gcache-test.el ends here
