;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(defmacro def-suite (name &key description in)
  "Define a new test-suite named NAME.

IN (a symbol), if provided, causes this suite te be nested in the
suite named by IN. NB: This macro is built on top of make-suite,
as such it, like make-suite, will overrwrite any existing suite
named NAME."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-suite ',name
                 ,@(when description `(:description ,description))
                 ,@(when in `(:in ',in)))
     ',name))

(defmacro def-suite* (name &rest def-suite-args)
  `(progn
     (def-suite ,name ,@def-suite-args)
     (in-suite ,name)))

(defun make-suite (name &key description in)
  "Create a new test suite object.

Overides any existing suite named NAME."
  (let ((suite (make-instance 'test-suite :name name)))
    (when description
      (setf (description suite) description))
    (loop :for i :in (ensure-list in)
          :for in-suite := (get-test i)
          :do (progn
                (when (null in-suite)
                  (cerror "Create a new suite named ~A." "Unknown suite ~A." i)
                  (setf (get-test in-suite) (make-suite i)
                        in-suite (get-test in-suite)))
                (setf (gethash name (tests in-suite)) suite)))
    (setf (get-test name) suite)
    suite))

(defvar *suite* (setf (get-test 'NIL)
                      (make-suite 'NIL :description "Global Suite"))
  "The current test suite object")

(defmacro in-suite (suite-name)
  "Set the *suite* special variable so that all tests defined
after the execution of this form are, unless specified otherwise,
in the test-suite named SUITE-NAME.

See also: DEF-SUITE *SUITE*"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%in-suite ,suite-name)))

(defmacro in-suite* (suite-name &key in)
  "Just like in-suite, but silently creates missing suites."
  `(%in-suite ,suite-name :in ,in :fail-on-error nil))

(defmacro %in-suite (suite-name &key (fail-on-error t) in)
  `(progn
     (aif (get-test ',suite-name)
          (setf *suite* it)
          (progn
            (when ,fail-on-error
              (cerror "Create a new suite named ~A."
                      "Unkown suite ~A." ',suite-name))
            (setf (get-test ',suite-name) (make-suite ',suite-name :in ',in)
                  *suite* (get-test ',suite-name))))
     ',suite-name))
