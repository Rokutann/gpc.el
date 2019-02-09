;;; gcache-test.el --- Tests for gcache

(require 'ert)
(require 'f)
(require 's)

(load (f-expand "gcache.el" default-directory))


;;; Setup functions.

(defun a-retriever ()
  t)

(defun setup-defcache-spec-orig ()
  (gcache-defcache-spec acache-spec
    "A new cache spec."
    '((current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
      (pwd "/" (lambda ()
                 (with-temp-buffer
                   (call-process "pwd" nil t)
                   (s-chop-suffix "\n" (buffer-string)))))
      (true nil a-retriever)))
  (defvar a-random-var nil))

(defun setup-defcache-spec ()
  (gcache-defcache-spec acache-spec
    "A new cache spec."
    (current-buffer-name "*scratch*" (lambda () (buffer-name (current-buffer))))
    (pwd "/" (lambda ()
               (with-temp-buffer
                 (call-process "pwd" nil t)
                 (s-chop-suffix "\n" (buffer-string)))))
    (true nil a-retriever))
  (defvar a-random-var nil))

(defun setup-gcache-defcache ()
  (setup-defcache-spec)
  (gcache-defcache g-cache acache-spec :doc-string "a global cache.")
  (gcache-defcache l-cache acache-spec :buffer-local t :doc-string "a buffer-local cache."))


;;; Tests.

(ert-deftest test-gcache-spec-get ()
  (setup-gcache-defcache)
  (should (equal (gcache-spec-get 'true g-cache)
                 '(nil a-retriever))))

(ert-deftest test-gcache-spec-set ()
  (setup-gcache-defcache)
  (should (equal (gcache-spec-get 'true g-cache)
                 '(nil a-retriever)))
  (gcache-spec-set 'true t 'a-fetchfn g-cache)
  (should (equal (gcache-spec-get 'true g-cache)
                 '(t a-fetchfn)))
  (gcache-spec-set 'test "testing" 'a-fetchfn g-cache)
  (should (equal (gcache-spec-get 'test g-cache)
                 '("testing" a-fetchfn)))
  )

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

(ert-deftest test-gcache-spec ()
  (setup-gcache-defcache)
  (should (equal (gcache-spec g-cache)
                 acache-spec)))

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

(ert-deftest test-gcache-util-copy-symbol-property ()
  (setq symbol-a 1)
  (setq symbol-b 2)
  (put 'symbol-a 'name 'value)
  (should (eq (gcache-util-copy-symbol-property 'name 'symbol-a 'symbol-b)
              'value)))


(ert-deftest test-gcache-util-make-alist-from-key-and-value0 ()
  (setq hash-a (make-hash-table))
  (puthash 'a '(1 10) hash-a)
  (puthash 'b '(2 20) hash-a)
  (should (gcache-alist-set-equal
           (gcache-util-make-alist-from-key-and-value0 hash-a)
           '((a . 1) (b . 2))))
  )

;;; Tests for alist functions.

(defun setup-alist ()
  (setq al '((a . b) (c . d)))
  (setq al-eql '((1 . a) (1.0 . b)))
  (setq al-equal '(("spam" . 3) ((a (b c)) . d))))

(ert-deftest test-gcache-alist-subset-p ()
  (should (eq (gcache-alist-subset-p '((a . 1)) '((b . 2) (a . 1)))
              t)))

(ert-deftest test-gcache-alist-equal ()
  (should (eq (gcache-alist-set-equal '((a . 1) (b . 2))
                                       '((b . 2) (a . 1)))
              t)))

(ert-deftest test-gcache-alist-clear ()
  (setup-alist)
  (gcache-alist-clear al)
  (should (eq al nil)))

(ert-deftest test-gcache-alist-get ()
  (setup-alist)
  (should (eq (gcache-alist-get 'a al)
              'b))
  (should (eq (gcache-alist-get 'b al)
              nil))
  (should (eq (gcache-alist-get 'c al)
              'd))
  (should (eq (gcache-alist-get 'a al :testfn 'eq)
              'b))
  (should (eq (gcache-alist-get 'b al :testfn 'eq)
              nil))
  (should (eq (gcache-alist-get 1 al-eql)
              'a))
  (should (eq (gcache-alist-get 1 al-eql :testfn 'eql)
              'a))
  (should (eq (gcache-alist-get 1.0 al-eql)
              nil))
  (should (eq (gcache-alist-get 1.0 al-eql :testfn 'eql)
              'b))
  (should (eq (gcache-alist-get "spam" al-equal :testfn 'equal)
              3))
  (should (eq (gcache-alist-get '(a (b c)) al-equal :testfn 'equal)
              'd))
  (should (eq (gcache-alist-get "Hi" al-equal :testfn #'(lambda (x y) (= (length x) (length y))))
              'd))
  (should (eq (gcache-alist-get 'a al :default 'no-value)
              'b))
  (should (eq (gcache-alist-get 'f al :default 'no-value)
              'no-value)))

(ert-deftest test-gcache-alist-set ()
  (setup-alist)
  (gcache-alist-set 'e 'f al)
  (should (gcache-alist-set-equal al '((a . b) (c . d) (e . f))))
  (gcache-alist-set 'c 'g al)
  (should (gcache-alist-set-equal al '((a . b) (c . g) (e . f))))
  )

(ert-deftest test-gcache-alist-remove ()
  (setup-alist)
  (gcache-alist-remove 'a al)
  (should (gcache-alist-set-equal al '((c . d))))
  )

(ert-deftest test-gcache-util-hash-to-alist ()
  (setq hash (make-hash-table))
  (puthash 'a 1 hash)
  (puthash 'b 2 hash)
  (should (gcache-alist-set-equal (gcache-util-hash-to-alist hash)
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
