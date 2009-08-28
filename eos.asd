;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :Eos
  :author "Adlai Chandrasekhar"
  :license "MIT"
  :depends-on (:arnesi)
  :components ((:static-file "eos.asd")
               (:module "src"
                        :components
                        ((:file "package")
                         (:file "classes" :depends-on ("package"))
                         (:file "check"   :depends-on ("package"))
                         (:file "fixture" :depends-on ("package"))
                         (:file "random"  :depends-on ("check"))
                         (:file "test"    :depends-on ("classes"))
                         (:file "explain" :depends-on ("classes" "check"))
                         (:file "suite"   :depends-on ("test"))
                         (:file "run"     :depends-on ("suite" "check"))))
               (:module "tests"
                        :depends-on ("src")
                        :components
                        ((:file "suite")
                         (:file "tests" :depends-on ("suite"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :Eos))))
  (funcall (intern (string :run!) (string :Eos)) :Eos))
