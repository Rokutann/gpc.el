;;; gcache-test.el --- Tests for gcache

(require 'ert)
(require 'f)

(load (f-expand "gcache.el" default-directory))


;;; Setup functions.

(defun a-retriever ()
  t)

(defun setup-defcache-spec ()
  (gcache-defcache-spec acache-spec
    "A new cache spec."
    '((current-buffer-name "*scratch*" '(lambda () (buffer-name (current-buffer))))
      (pwd "/" '(lambda () (with-temp-buffer
                        (call-process "pwd" nil t)
                        (buffer-string))))
      (true nil a-retriever)))
  (defvar a-random-var nil))

(defun setup-gcache-defcache ()
  (setup-defcache-spec)
  (gcache-defcache g-cache acache-spec :doc-string "a global cache.")
  (gcache-defcache l-cache acache-spec :buffer-local t :doc-string "a buffer-local cache."))


;;; Tests.

(ert-deftest test-def-cache-spec ()
  (setup-defcache-spec)
  ;;(should (eq (hash-table-p acache-spec) t))
  (should (eq (gcache-cache-spec-p acache-spec) t))
  (should (eq (gcache-cache-spec-p a-random-var) nil))
  (should (equal (nth 0 (gethash 'current-buffer-name acache-spec)) "*scratch*"))
  (should (equal (nth 0 (gethash 'pwd acache-spec)) "/")))

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

(ert-deftest test-gcache-exist-p ()
  (setup-gcache-defcache)
  (should (eq (gcache-exist-p 'spam g-cache) nil))
  (should (eq (gcache-exist-p 'pwd g-cache) t)))

(ert-deftest test-gcache-fetch ()
  (setup-gcache-defcache)
  (should (equal (gcache-fetch 'pwd g-cache) "/"))
  (gcache-clear g-cache)
  (should (eq (gcache-fetch 'pwd g-cache) 'no-value)))

(ert-deftest test-gcache-defcache ()
  (setup-defcache-spec)
  (gcache-defcache acache acache-spec)
  ;; (should (eq (hash-table-p acache)
  ;;             t))
  (should (eq (get 'acache 'gcache-cache-spec)
              'acache-spec))
  ;; (should (equal (gethash 'pwd acache)
  ;;                "/"))
  ;; (should (equal (gethash 'pwd acache)
  ;;                "/"))
  (should (equal (gcache-fetch 'pwd acache) "/"))
  )

(ert-deftest test-gcache-clear ()
  (setup-gcache-defcache)
  (gcache-clear g-cache)
  (should (eq g-cache nil)))

(ert-deftest test-gcache--get-retrieve-fun ()
  (setup-gcache-defcache)
  (should (eq (gcache--get-retrieve-fun 'true g-cache)
              'a-retriever)))


;;; Tests for helper functions.

(ert-deftest test-gcache--copy-symbol-property ()
  (setq symbol-a 1)
  (setq symbol-b 2)
  (put 'symbol-a 'name 'value)
  (should (eq (gcache--copy-symbol-property 'name 'symbol-a 'symbol-b)
              'value)))

(ert-deftest test-gcache--copy-keys-and-value-0 ()
  (setq hash-a (make-hash-table))
  (setq hash-b (make-hash-table))
  (puthash 'a '(1 10) hash-a)
  (puthash 'b '(2 20) hash-a)
  (gcache--copy-keys-and-value-0 hash-a hash-b)
  (should (eq (gethash 'a hash-b)
              1))
  (should (eq (gethash 'b hash-b)
              2))
  (should (eq (gethash 'c hash-b)
              nil))
  )


(ert-deftest test-gcache--make-alist-from-key-and-value0 ()
  (setq hash-a (make-hash-table))
  (puthash 'a '(1 10) hash-a)
  (puthash 'b '(2 20) hash-a)
  (should (alist-equal
           (gcache--make-alist-from-key-and-value0 hash-a)
           '((a . 1) (b . 2))))
  )


;;; Tests for test helper functions.

(ert-deftest test-is-subset-alist-of ()
  (should (eq (is-subset-alist-of '((a . 1)) '((b . 2) (a . 1)))
              t)))

(ert-deftest test-alist-equal ()
  (should (eq (alist-equal '((a . 1) (b . 2))
                           '((b . 2) (a . 1)))
              t)))

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
