;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :Eos
  :author "Adlai Chandrasekhar"
  :license "MIT"
  :depends-on (:arnesi)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "utils"   :depends-on ("package"))
                         (:file "classes" :depends-on ("package"))
                         (:file "check"   :depends-on ("utils"))
                         (:file "fixture" :depends-on ("package"))
                         (:file "random"  :depends-on ("check"))
                         (:file "test"    :depends-on ("classes"))
                         (:file "explain" :depends-on ("classes" "check"))
                         (:file "suite"   :depends-on ("test" "utils"))
                         (:file "run"     :depends-on ("suite" "check"))))))

(asdf:defsystem :Eos-tests
  :author "Adlai Chandrasekhar"
  :license "MIT"
  :depends-on (:Eos)
  :components ((:module "tests"
                         :components
                         ((:file "suite")
                          (:file "tests" :depends-on ("suite"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :Eos))))
  (format t "~&~%*******************~%~
                 ** Loading tests **~%~
                 *******************~%")
  (asdf:oos 'asdf:load-op :Eos-tests)
  (asdf:oos 'asdf:test-op :Eos-tests))
