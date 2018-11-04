(in-package #:cartesian-product-switch)

(defun %map-with-index (mapping-function function arg)
  (let ((index -1))
    (funcall mapping-function
             (lambda (arg)
               (funcall function arg (incf index)))
             arg)))

(define (testclause svref) (possibilities-count index)
  (check-type possibilities-count (integer 0))
  (values (let ((index-var (gensym (string '#:index))))
            `(let ((,index-var ,index))
               (check-type ,index-var (integer 0 (,possibilities-count)))))
          possibilities-count
          t))

(define (testclause cond) (&body tests)
  (values `(cond ,@(%map-with-index #'mapcar #'list tests))
          (length tests)
          t))

(dolist (operator-group '((case ccase ecase) (typecase ctypecase etypecase)))
  (let ((group-name (first operator-group)))
    (dolist (operator operator-group)
      (%ensure
       operator
       (list (if (member operator '(ccase ctypecase))
                 'keyplace
                 'keyform)
             '&body
             (if (eq group-name 'case)
                 'keygroups
                 'tests))
       (lambda (form env)
         (declare (ignore env))
         (destructuring-bind (keyform &body tests) (cdr form)
           (values `(,operator
                     ,keyform
                     ,@(%map-with-index #'mapcar #'list tests))
                   (length tests)
                   (member operator '(case typecase)))))))))

(define (testclause if) (test)
  (values `(if ,test 0 1) 2 nil))
