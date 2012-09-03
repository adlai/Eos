;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(def-suite Eos)

#+asdf
(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :Eos-tests))))
  (format t "~2&********************~@
                ** Starting tests **~@
                ********************~%")
  (run! 'Eos)
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~@
                ** If there were any failures on your  **~@
                ** platform, please report them to me: **~@
                **    (munchking at gmail dot com)     **~@
                ** or just file a bugreport on github: **~@
                **     github.com/adlai/Eos/issues     **~@
                *****************************************~%"))

#-asdf
(defun run-self-tests ()
  (format t "~2&********************~@
                ** Starting tests **~@
                ********************~%")
  (run! 'Eos)
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~@
                ** If there were any failures on your  **~@
                ** platform, please report them to me: **~@
                **    (munchking at gmail dot com)     **~@
                ** or just file a bugreport on github: **~@
                **     github.com/adlai/Eos/issues     **~@
                *****************************************~%"))
