;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(def-suite Eos)

(defun run-all-tests ()
  (run! 'Eos))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :Eos-tests))))
  (format t "~&~%********************~%~
                 ** Starting tests **~%~
                 ********************~%")
  (run-all-tests)
  (format t "~&*****************************************~%~
               **            Tests finished           **~%~
               *****************************************~%~
               ** If there were any failures on your  **~%~
               ** platform, please report them to me: **~%~
               **    (munchking at gmail dot com)     **~%~
               ** or just file a bugreport on github: **~%~
               **     github.com/adlai/Eos/issues     **~%~
               *****************************************~%"))