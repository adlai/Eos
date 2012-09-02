(defsystem eos (:default-pathname "src")
  :members ("package"
            "utils"
            "classes"
            "check"
            "test"
            "explain"
            "suite"
            "run")
  :rules ((:in-order-to :compile ("utils" "classes")
           (:requires (:load "package")))
          (:in-order-to :compile "check"
           (:requires (:load "utils")))
          (:in-order-to :compile "explain"
           (:requires
            (:load "classes")
            (:load "check")))
          (:in-order-to :compile "test"
           (:requires (:load "classes")))
          (:in-order-to :compile "suite"
           (:requires
            (:load "test")
            (:load "utils")))
          (:in-order-to :load "run"
           (:requires
            (:load "suite")
            (:load "check")))))

(defsystem eos-tests (:default-pathname "tests")
  :members ("suite"
            "tests")
  :rules ((:in-order-to :compile "tests"
           (:requires (:load "suite")))))
