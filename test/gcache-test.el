;;; gcache-test.el --- Tests for gcache

(require 'ert)
(require 'f)

(load (f-expand "gcache.el" default-directory))

(defun setup-defchache ()
  (defcache g-cache :doc-string "a global cache.")
  (defcache l-cache :buffer-local t :doc-string "a buffer-local cache."))

(defun setup-defcache-spec ()
  (gcache-defcache-spec acache-spec
                        "A new cache spec."
                        '((current-buffer-name "*scratch*" '(lambda () (buffer-name (current-buffer))))
                          (pwd "/" '(lambda () (with-temp-buffer
                                            (call-process "pwd" nil t)
                                            (buffer-string))))))
  (defvar a-random-var nil))

(ert-deftest test-defcache-buffer-local ()
  (setup-defchache)
  (should (eq (local-variable-if-set-p 'g-cache) nil))
  (should (eq (local-variable-if-set-p 'l-cache) t)))

(ert-deftest test-defcache-docstring ()
  (setup-defchache)
  (should (equal (documentation-property 'g-cache 'variable-documentation)
                 "a global cache."))
  (should (equal (documentation-property 'l-cache 'variable-documentation)
                 "a buffer-local cache.")))

(ert-deftest test-exist-p ()
  (setup-defchache)
  (should (eq (gcache-exist-p 'a-cache-entry g-cache) nil))
  (should (eq (gcache-add a-cache-entry #'(lambda (x) x) g-cache) 'a-cache-entry))
  (should (eq (gcache-exist-p 'a-cache-entry g-cache) t)))

(ert-deftest test-def-cache-spec ()
  (setup-defcache-spec)
  (should (eq (hash-table-p acache-spec) t))
  (should (eq (gcache-cache-spec-p acache-spec) t))
  (should (eq (gcache-cache-spec-p a-random-var) nil))
  (should (equal (nth 0 (gethash 'current-buffer-name acache-spec)) "*scratch*"))
  (should (equal (nth 0 (gethash 'pwd acache-spec)) "/")))




;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; gcache-test.el ends here
