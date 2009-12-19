;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defmacro fn (&body body)
  "An abbreviation for some common lambda use-cases."
  (let ((parameter (intern "_")))
    `(lambda (,parameter) (declare (ignorable ,parameter)) ,@body)))

(defmacro aif (test true &optional false)
  `(let ((it ,test))
     (if it ,true ,false)))

(defun parallel-lookup (thing key-list value-list &key (test #'eql) (key #'identity))
  (map nil (lambda (k v)
             (when (funcall test thing (funcall key k))
               (return-from parallel-lookup v)))
       key-list value-list))

(defmacro pushend (new-item list list-end &environment env)
  (multiple-value-bind (list.gvars list.vals list.gstorevars list.setter list.getter)
      (get-setf-expansion list env)
    (multiple-value-bind (tail.gvars tail.vals tail.gstorevars tail.setter tail.getter)
        (get-setf-expansion list-end env)
      (let ((gitem (gensym))
            (list.gstorevar (first list.gstorevars))
            (tail.gstorevar (first tail.gstorevars)))
        `(let (,@(mapcar #'list list.gvars list.vals)
               ,@(mapcar #'list tail.gvars tail.vals))
           (let ((,gitem (list ,new-item)))
             (if ,list.getter
                 (let ((,tail.gstorevar ,gitem))
                   (setf (cdr ,tail.getter) ,gitem)
                   ,tail.setter)
                 (let ((,list.gstorevar ,gitem)
                       (,tail.gstorevar ,gitem))
                   ,list.setter ,tail.setter))))))))

(defmacro with-push-onto ((&rest places) &body body)
  (let ((end-names (mapcar (fn (gensym (symbol-name _))) places)))
    `(let (,@places ,@end-names)
       (macrolet ((push-onto (place thing)
                    `(pushend ,thing ,place
                              ,(parallel-lookup place ',places ',end-names))))
         ,@body))))

(defmacro with-gensyms ((&rest syms) &body body)
  "This is a simple WITH-GENSYMS, similar to the one presented in PCL."
  `(let ,(mapcar (fn `(,_ (gensym ,(string _)))) syms) ,@body))

;;; This is based on from Arnesi's src/list.lisp, and implements a naive ;;; list matching facility.
;;; Marco Baringer says in the original:
;;; ;;;; ** Simple list matching based on code from Paul Graham's On Lisp.

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-gensyms (val foundp)
        (destructuring-bind ((test &rest progn) &rest others)
            clauses
          `(multiple-value-bind (,val ,foundp)
               ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))

(defun varsymp (x)
  (and (symbolp x) (eq (aref (symbol-name x) 0) #\?)))

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

(defun vars (match-spec)
  (let ((vars nil))
    (labels ((find-vars (spec)
               (cond
                 ((null spec) nil)
                 ((varsymp spec) (push spec vars))
                 ((consp spec)
                  (find-vars (car spec))
                  (find-vars (cdr spec))))))
      (find-vars match-spec))
    (delete-duplicates vars)))

(defmacro list-match-case (target &body clauses)
  (if clauses
      (destructuring-bind ((test &rest progn) &rest others)
          clauses
        (with-gensyms (tgt binds success)
          `(let ((,tgt ,target))
             (multiple-value-bind (,binds ,success)
                 (list-match ,tgt ',test)
               (declare (ignorable ,binds))
               (if ,success
                   (let ,(mapcar (lambda (var)
                                   `(,var (cdr (assoc ',var ,binds))))
                                 (vars test))
                     (declare (ignorable ,@(vars test)))
                     ,@progn)
                   (list-match-case ,tgt ,@others))))))
      nil))
