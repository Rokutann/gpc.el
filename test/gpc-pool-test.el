;;; gpc-pool-test.el --- Tests for gpc-pool functions.

;;; Tests.


(ert-deftest gpc-pool-init ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (should (member 'pipenv-virtualenvs (symbol-plist 'npy-env))))

(ert-deftest gpc-pool-pushnew ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (equal (get 'npy-env 'pipenv-virtualenvs) '("/usr/local/project1"))))

(ert-deftest gpc-pool-pushnew/test-same ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (equal (get 'npy-env 'pipenv-virtualenvs) '("/usr/local/project1"))))

(ert-deftest gpc-pool-pushnew/test-different ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'eq)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'eq)
  (should (equal (get 'npy-env 'pipenv-virtualenvs) '("/usr/local/project1" "/usr/local/project1"))))

(ert-deftest gpc-pool-clear ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-clear 'pipenv-virtualenvs npy-env)
  (should (equal (get 'npy-env 'pipenv-virtualenvs) nil)))

(ert-deftest gpc-pool-get-all ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-helper-seq-set-equal-p (gpc-pool-get-all 'pipenv-virtualenvs npy-env)
                                      '("/usr/local/project1" "/usr/local/project2"))))

(ert-deftest gpc-pool-map ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (let ((result))
    (gpc-pool-map #'(lambda (x) (push x result)) 'pipenv-virtualenvs npy-env)
    (should (gpc-helper-seq-set-equal-p result
                                        '("/usr/local/project1" "/usr/local/project2")))))

(ert-deftest gpc-pool-member/exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-pool-member "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)))

(ert-deftest gpc-pool-member/not-exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should-not (gpc-pool-member "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'eql)))

(ert-deftest gpc-pool-member-if/exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/share/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-pool-member-if #'(lambda (x) (s-matches-p (concat "^" x)
                                                    "/usr/local/project1/foo.py"))
                              'pipenv-virtualenvs npy-env)))

(ert-deftest gpc-pool-member-if/not-exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/share/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should-not (gpc-pool-member-if #'(lambda (x) (s-matches-p (concat "^" x)
                                                        "/usr/local/"))
                                  'pipenv-virtualenvs npy-env)))

(ert-deftest gpc-pool-delete/exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-delete "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-helper-seq-set-equal-p
           (gpc-pool-get-all 'pipenv-virtualenvs npy-env) '("/usr/local/project1"))))

(ert-deftest gpc-pool-delete/not-exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-delete "/usr/local/project3" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-helper-seq-set-equal-p (gpc-pool-get-all 'pipenv-virtualenvs npy-env)
                                      '("/usr/local/project1" "/usr/local/project2"))))

(ert-deftest gpc-pool-member-if/exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/share/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-pool-member-if #'(lambda (x) (s-matches-p (concat "^" x)
                                                    "/usr/local/project1/foo.py"))
                              'pipenv-virtualenvs npy-env)))

(ert-deftest gpc-pool-delete/exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-delete "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-helper-seq-set-equal-p
           (gpc-pool-get-all 'pipenv-virtualenvs npy-env) '("/usr/local/project1"))))

(ert-deftest gpc-pool-delete/not-exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-delete "/usr/local/project3" 'pipenv-virtualenvs npy-env :test 'equal)
  (should (gpc-helper-seq-set-equal-p (gpc-pool-get-all 'pipenv-virtualenvs npy-env)
                                      '("/usr/local/project1" "/usr/local/project2"))))


(ert-deftest gpc-pool-delete-if/exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/share/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project1/src" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/local/project1/doc" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-delete-if #'(lambda (x) (s-matches-p "^/usr/local/project1" x))
                      'pipenv-virtualenvs npy-env)
  (should (gpc-helper-seq-set-equal-p
           (gpc-pool-get-all 'pipenv-virtualenvs npy-env)
           '("/usr/share/project2"))))

(ert-deftest gpc-pool-delete-if/not-exists ()
  (gpc-pool-init 'pipenv-virtualenvs npy-env)
  (gpc-pool-pushnew "/usr/local/project1" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-pushnew "/usr/share/project2" 'pipenv-virtualenvs npy-env :test 'equal)
  (gpc-pool-delete-if #'(lambda (x) (s-matches-p "^/usr/bin/" x))
                      'pipenv-virtualenvs npy-env)
  (should (gpc-helper-seq-set-equal-p
           (gpc-pool-get-all 'pipenv-virtualenvs npy-env)
           '("/usr/local/project1" "/usr/share/project2"))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; gpc-pool-test.el ends here
