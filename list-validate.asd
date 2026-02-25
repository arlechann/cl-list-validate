(defsystem "list-validate"
  :version "1.0.0"
  :author "arlechann"
  :license "CC0-1.0"
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "validate")
                 (:file "validator-rules")
                 (:file "parser-rules"))))
  :description "List validation and parsing utilities"
  :in-order-to ((test-op (test-op "list-validate/tests"))))

(defsystem "list-validate/tests"
  :author "arlechann"
  :license "CC0-1.0"
  :depends-on (:list-validate
               :rove)
  :components ((:module "tests"
                :components
                ((:file "validate"))))
  :description "Test system for list-validate"
  :perform (test-op (op c) (symbol-call :rove :run c)))
