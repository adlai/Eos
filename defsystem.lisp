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
           (:requires (:load "classes")
                      (:load "check")))
          (:in-order-to :compile "test"
           (:requires (:load "classes")))
          (:in-order-to :compile "suite"
           (:requires (:load "test")
                      (:load "utils")))
          (:in-order-to :compile "run"
           (:requires (:load "explain")
                      (:load "suite")
                      (:load "check")))))

(defsystem eos-tests (:default-pathname "tests")
  :members (("eos" :type :system)
            "suite"
            "tests")
  :rules ((:in-order-to :compile :all
           (:caused-by (:compile "eos"))
           (:requires  (:load "eos")))
          (:in-order-to :compile "tests"
           (:requires (:load "suite")))))
