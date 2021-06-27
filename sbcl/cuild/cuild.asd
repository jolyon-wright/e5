(defsystem "cuild"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cuild/tests"))))

(defsystem "cuild/tests"
  :author ""
  :license ""
  :depends-on ("cuild"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cuild"
  :perform (test-op (op c) (symbol-call :rove :run c)))
