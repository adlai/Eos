;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(defpackage :Eos
  (:use :common-lisp)
  (:export ;; creating tests and test-suites
           #:make-suite
	   #:def-suite
	   #:in-suite
	   #:in-suite*
	   #:make-test
	   #:test
	   #:get-test
	   #:rem-test
           #:test-names
	   ;; running checks
           #:is
           #:is-every
           #:is-true
           #:is-false
           #:signals
           #:finishes
           #:skip
	   #:pass
	   #:fail
	   #:*test-dribble*
	   ;; running tests
           #:run
           #:run-all-tests
           #:explain
           #:explain!
           #:run!
           #:debug!
           #:!
           #:!!
           #:!!!
           #:*run-test-when-defined*
	   #:*debug-on-error*
           #:*debug-on-failure*
           #:*verbose-failures*
           #:results-status))

(pushnew :Eos *features*)
