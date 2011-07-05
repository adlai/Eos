;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(defun ensure-list (x)
  (if (listp x) x (list x)))

;; Make sure these macros are available when compiling this file
(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro fun (&body body)
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defmacro aif (test true &optional false)
  `(let ((it ,test))
     (if it ,true ,false)))

(defmacro with-gensyms ((&rest syms) &body body)
  "This is a simple WITH-GENSYMS, similar to the one presented in PCL."
  `(let ,(mapcar (fun `(,_ (gensym ,(string _)))) syms) ,@body))

(defmacro collect (names &body body &aux macros binds)
  (dolist (name names)
    (with-gensyms (value tail)
      (setf binds (list* value tail binds))
      (push `(,name (&optional (form nil formp))
               `(if ,formp
                    (let ((cons (list ,form)))
                       (cond ((null ,',tail)
                              (setf ,',tail  cons ,',value cons))
                             (t (setf (cdr ,',tail) cons ,',tail cons))))
                    ,',value))
            macros)))
  `(let* ,(nreverse binds) (macrolet ,macros ,@body)))

;;; This is based on from Arnesi's src/list.lisp, and implements a naive list matching facility.
;;; Marco Baringer says in the original:
;;; "Simple list matching based on code from Paul Graham's On Lisp."

(defmacro acond2 (&rest clauses)
  (when clauses
    (with-gensyms (val foundp)
      (destructuring-bind ((test &body progn) &rest others) clauses
        `(multiple-value-bind (,val ,foundp) ,test
           (if (or ,val ,foundp)
               (let ((it ,val))
                 (declare (ignorable it))
                 ,@progn)
               (acond2 ,@others)))))))

) ;; End of eval-when

(defun varsymp (x)
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun list-match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_))
     (values binds t))
    ((binding x binds) (list-match it y binds))
    ((binding y binds) (list-match x it binds))
    ((varsymp x) (values (cons (cons x y) binds) t))
    ((varsymp y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (list-match (car x) (car y) binds))
     (list-match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defun find-vars (spec acc)
  (typecase spec
    (cons (find-vars (car spec) (find-vars (cdr spec) acc)))
    ((satisfies varsymp) (adjoin spec acc :test #'eq))
    (otherwise acc)))

(defmacro list-match-case (target &body clauses)
  (when clauses
    (destructuring-bind ((test &body progn) &rest others) clauses
      (with-gensyms (tgt binds success)
        (let ((vars (find-vars test nil)))
          `(let ((,tgt ,target)) ; (once-only (target) ..)
             (multiple-value-bind (,binds ,success)
                 (list-match ,tgt ',test)
               (declare (ignorable ,binds))
               (if ,success
                   (let ,(mapcar (fun `(,_ (cdr (assoc ',_ ,binds)))) vars)
                     (declare (ignorable ,@vars))
                     ,@progn)
                   (list-match-case ,tgt ,@others)))))))))
