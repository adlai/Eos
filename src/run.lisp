;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(defparameter *debug-on-error* nil
  "T if we should drop into a debugger on error, NIL otherwise.")

(defparameter *debug-on-failure* nil
  "T if we should drop into a debugger on a failing check, NIL otherwise.")

(defun import-testing-symbols (package-designator)
  (import '(Eos::is Eos::is-true Eos::is-false Eos::signals Eos::finishes)
          package-designator))

(defparameter *run-queue* '()
  "List of test waiting to be run.")

(define-condition circular-dependency (error)
  ((test-case :initarg :test-case))
  (:report (lambda (cd stream)
             (format stream "A circular dependency wes detected in ~S." (slot-value cd 'test-case))))
  (:documentation "Condition signaled when a circular dependency
between test-cases has been detected."))

(defgeneric run-resolving-dependencies (test)
  (:documentation "Given a dependency spec determine if the spec
is satisfied or not, this will generally involve running other
tests. If the dependency spec can be satisfied the test is alos
run."))

(defmethod run-resolving-dependencies ((test test-case))
  "Return true if this test, and its dependencies, are satisfied,
  NIL otherwise."
  (case (status test)
    (:unknown
     (setf (status test) :resolving)
     (if (or (not (depends-on test))
             (eql t (resolve-dependencies (depends-on test))))
         (progn
           (run-test-lambda test)
           (status test))
         (with-run-state (result-list)
           (unless (eql :circular (status test))
             (push (make-instance 'test-skipped
                                  :test-case test
                                  :reason "Dependencies not satisfied")
                   result-list)
             (setf (status test) :depends-not-satisfied)))))
    (:resolving
     (restart-case
         (error 'circular-dependency :test-case test)
       (skip ()
         :report (lambda (s)
                   (format s "Skip the test ~S and all its dependencies." (name test)))
         (with-run-state (result-list)
           (push (make-instance 'test-skipped :reason "Circular dependencies" :test-case test)
                 result-list))
         (setf (status test) :circular))))
    (t (status test))))

(defmethod resolve-dependencies ((depends-on symbol))
  "A test which depends on a symbol is interpreted as `(AND
  ,DEPENDS-ON)."
  (run-resolving-dependencies (get-test depends-on)))

(defmethod resolve-dependencies ((depends-on list))
  "Return true if the dependency spec DEPENDS-ON is satisfied,
  nil otherwise."
  (if (null depends-on)
      t
      (flet ((satisfies-depends-p (test)
               (funcall test (lambda (dep)
                               (eql t (resolve-dependencies dep)))
                             (cdr depends-on))))
        (ecase (car depends-on)
          (and (satisfies-depends-p #'every))
          (or  (satisfies-depends-p #'some))
          (not (satisfies-depends-p #'notany))))))

(defun results-status (result-list)
  "Given a list of test results (generated while running a test)
  return true if all of the results are of type TEST-PASSED,
  faile otherwise."
  (every (lambda (res)
           (typep res 'test-passed))
         result-list))

(defun return-result-list (test-lambda)
  "Run the test function TEST-LAMBDA and return a list of all
  test results generated, does not modify the special environment
  variable RESULT-LIST."
  (bind-run-state ((result-list '()))
    (funcall test-lambda)
    result-list))

(defmethod run-test-lambda ((test test-case))
  (with-run-state (result-list)
    (bind-run-state ((current-test test))
      (labels ((abort-test (e)
                 (add-result 'unexpected-test-failure
                             :test-expr nil
                             :test-case test
                             :reason (format nil "Unexpected Error: ~S~%~A." e e)
                             :condition e))
               (run-it ()
                 (let ((result-list '()))
                   (declare (special result-list))
                   (handler-bind ((check-failure (lambda (e)
                                                   (declare (ignore e))
                                                   (unless *debug-on-failure*
                                                     (invoke-restart
                                                      (find-restart 'ignore-failure)))))
                                  (error (lambda (e)
                                           (unless (or *debug-on-error*
                                                       (typep e 'check-failure))
                                             (abort-test e)
                                             (return-from run-it result-list)))))
                     (restart-case
                         (let ((*readtable* (copy-readtable))
                               (*package* (runtime-package test)))
                           (funcall (test-lambda test)))
                       (retest ()
                         :report (lambda (stream)
                                   (format stream "~@<Rerun the test ~S~@:>" test))
                         (return-from run-it (run-it)))
                       (ignore ()
                         :report (lambda (stream)
                                   (format stream "~@<Signal an exceptional test failure and abort the test ~S.~@:>" test))
                         (abort-test (make-instance 'test-failure :test-case test
                                                    :reason "Failure restart."))))
                     result-list))))
        (let ((results (run-it)))
          (setf (status test) (results-status results)
                result-list (nconc result-list results)))))))

(defgeneric %run (test-spec)
  (:documentation "Internal method for running a test. Does not
  update the status of the tests nor the special vairables !,
  !!, !!!"))

(defmethod %run ((test test-case))
  (run-resolving-dependencies test))

(defmethod %run ((tests list))
  (mapc #'%run tests))

(defmethod %run ((suite test-suite))
  (let (suite-results)
    (flet ((run-tests ()
             (loop
                :for test :being :the hash-values :of (tests suite)
                :do (%run test))))
      (unwind-protect
           (bind-run-state ((result-list '()))
             (run-tests)
             (setf suite-results result-list
                   (status suite) (every (fun (typep _ 'test-passed)) suite-results)))
        (with-run-state (result-list)
          (setf result-list (nconc result-list suite-results)))))))

(defmethod %run ((test-name symbol))
  (when (get-test test-name)
    (%run (get-test test-name))))

(defvar *initial-!* (lambda () (format t "Haven't run that many tests yet.~%")))
(defvar *!* *initial-!*)
(defvar *!!* *initial-!*)
(defvar *!!!* *initial-!*)

(defun run! (&optional (test-spec *suite*))
  "Equivalent to (explain (run TEST-SPEC))."
  (explain! (run test-spec)))

(defun explain! (result-list)
  "Explain the results of RESULT-LIST using a
detailed-text-explainer with output going to *test-dribble*"
  (explain (make-instance 'detailed-text-explainer) result-list *test-dribble*))

(defun debug! (&optional (test-spec *suite*))
  "Calls (run! test-spec) but enters the debugger if any kind of error happens."
  (let ((*debug-on-error* t)
        (*debug-on-failure* t))
    (run! test-spec)))

(defun run (test-spec)
  "Run the test specified by TEST-SPEC.

TEST-SPEC can be either a symbol naming a test or test suite, or
a testable-object object. This function changes the operations
performed by the !, !! and !!! functions."
  (psetf *!* (lambda ()
               (loop :for test :being :the :hash-keys :of *test*
                     :do (setf (status (get-test test)) :unknown))
               (bind-run-state ((result-list '()))
                 (with-simple-restart (explain "Ignore the rest of the tests and explain current results")
                   (%run test-spec))
                 result-list))
         *!!* *!*
         *!!!* *!!*)
  (funcall *!*))

(defun ! ()
  "Rerun the most recently run test and explain the results."
  (explain! (funcall *!*)))

(defun !! ()
  "Rerun the second most recently run test and explain the results."
  (explain! (funcall *!!*)))

(defun !!! ()
  "Rerun the third most recently run test and explain the results."
  (explain! (funcall *!!!*)))
