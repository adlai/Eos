;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(defvar *fixture* (build-hash-table '(:test eql) nil)
  "Table mapping fixture names to fixture objects.")

(defun get-fixture (key &optional default)
  (gethash key *fixture* default))

(defun (setf get-fixture) (value key)
  (when (gethash key *fixture*)
    (warn "Redefining fixture ~a" key))
  (setf (gethash key *fixture*) value))

(defun rem-fixture (key)
  (remhash key *fixture*))

(defmacro def-fixture (name args &body body)
  "Defines a fixture named NAME. A fixture is very much like a
macro but is used only for simple templating. A fixture created
with DEF-FIXTURE is a macro which can use the special macrolet
&BODY to specify where the body should go.

See Also: `with-fixture'."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get-fixture ',name) (cons ',args ',body))
     ',name))

(defmacro with-fixture (fixture-name args &body body)
  "Insert BODY into the fixture named FIXTURE-NAME.

See Also: `def-fixture'."
  (assert (get-fixture fixture-name) (fixture-name)
          "Unknown fixture ~S." fixture-name)
  (destructuring-bind (largs &rest lbody) (get-fixture fixture-name)
    `(macrolet ((&body () '(progn ,@body)))
       (funcall (lambda ,largs ,@lbody) ,@args))))