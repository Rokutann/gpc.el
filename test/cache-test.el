;;; cache-test.el --- Tests for cache

(require 'ert)
(require 'f)

(load (f-expand "cache.el" default-directory))

(defun setup ()
  (defcache g-cache :docstring "a global cache.")
  (defcache l-cache :buffer-local t :docstring "a buffer-local cache."))

(ert-deftest test-defcache-buffer-local ()
  (setup)
  (should (eq (local-variable-if-set-p 'g-cache) nil))
  (should (eq (local-variable-if-set-p 'l-cache) t)))

(ert-deftest test-defcache-docstring ()
  (setup)
  (should (equal (documentation-property 'g-cache 'variable-documentation)
                 "a global cache."))
  (should (equal (documentation-property 'l-cache 'variable-documentation)
                 "a buffer-local cache.")))

(ert-deftest test-exist-p ()
  (setup)
  (should (eq (cache-exist-p 'a-cache-entry g-cache) nil))
  (should (eq (cache-add a-cache-entry #'(lambda (x) x) g-cache) 'a-cache-entry))
  (should (eq (cache-exist-p 'a-cache-entry g-cache) t)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; cache-test.el ends here
