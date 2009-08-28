;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(deflookup-table test
  :at-redefinition nil
  :documentation "Lookup table mapping test (and test suite)
  names to objects.")

(defun test-names ()
  (loop for test being the hash-keys of *test* collect test))

(defmacro test (name &body body)
  "Create a test named NAME. If NAME is a list it must be of the
form:

  (name &key depends-on suite fixture compile-at profile)

NAME is the symbol which names the test.

DEPENDS-ON is a list of the form:

 (AND . test-names) - This test is run only if all of the tests
 in TEST-NAMES have passed, otherwise a single test-skipped
 result is generated.

 (OR . test-names) - If any of TEST-NAMES has passed this test is
 run, otherwise a test-skipped result is generated.

 (NOT test-name) - This is test is run only if TEST-NAME failed.

AND, OR and NOT can be combined to produce complex dependencies.

If DEPENDS-ON is a symbol it is interpreted as `(AND
,depends-on), this is accomadate the common case of one test
depending on another.

FIXTURE specifies a fixtrue to wrap the body in.

If PROFILE is T profiling information will be collected as well."
  (let* ((tmp (gensym))
         (suite-arg (getf (cdr (ensure-list name)) :suite tmp))
         (suite-form (if (eq tmp suite-arg) '*suite*
                         `(get-test ',suite-arg))))
    (when (consp name) (remf (cdr name) :suite))
    (destructuring-bind
          (name &key depends-on (compile-at :run-time) fixture profile)
        (ensure-list name)
      (declare (type (member :run-time :definition-time) compile-at))
      (let ((description (if (stringp (car body)) (pop body) ""))
            (effective-body (if fixture
                                (destructuring-bind (name &rest args)
                                    (ensure-list fixture)
                                  `((with-fixture ,name ,args ,@body)))
                                body)))
        `(progn
           (setf (get-test ',name)
                 (make-instance 'test-case
                                :name ',name
                                :runtime-package
                                #-ecl ,*package*
                                #+ecl (find-package ,(package-name *package*))
                                :test-lambda
                                (lambda ()
                                  ,@ (ecase compile-at
                                       (:run-time `((funcall
                                                     (let ((*package* (find-package ',(package-name *package*))))
                                                       (compile nil '(lambda ()
                                                                      ,@effective-body))))))
                                       (:definition-time effective-body)))
                                :description ,description
                                :depends-on ',depends-on
                                :collect-profiling-info ,profile))
           (setf (gethash ',name (tests ,suite-form)) ',name)
           (when *run-test-when-defined* (run! ',name))
           ',name)))))

(defvar *run-test-when-defined* nil
  "When non-NIL tests are run as soon as they are defined.")
