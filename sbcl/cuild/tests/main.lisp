(defpackage cuild/tests/main
  (:use :cl
        :cuild
        :rove))
(in-package :cuild/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cuild)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
