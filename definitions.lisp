(in-package #:cartesian-product-switch)

(defun %map-with-index (mapping-function function arg)
  (let ((index -1))
    (funcall mapping-function
             (lambda (arg)
               (funcall function arg (incf index)))
             arg)))

(define (cp-switch:standard-testclause-kind if) (test)
  (values `(if ,test 0 1) 2 nil))

(define (cp-switch:standard-testclause-kind svref) (possibilities-count index)
  (check-type possibilities-count (integer 0))
  (values (let ((index-var (gensym (string '#:index))))
            `(let ((,index-var ,index))
               (check-type ,index-var (integer 0 (,possibilities-count)))))
          possibilities-count
          t))

(define (cp-switch:standard-testclause-kind cond) (&body tests)
  (values `(cond ,@(%map-with-index #'mapcar #'list tests))
          (length tests)
          t))

(defmacro %define-caselikes ((caselike ccaselike ecaselike))
  (let ((clause-test-var (if (eq caselike 'case)
                             'keygroups
                             'tests)))
    (flet ((%def (operator)
             (let* ((key-var (if (member operator '(ccase ctypecase))
                                 'keyplace
                                 'keyform))
                    (lambda-list `(,key-var &body ,clause-test-var)))
               `(define (cp-switch:standard-testclause-kind ,operator) ,lambda-list
                  (values (list* ',operator
                                 ,key-var
                                 (%map-with-index #'mapcar #'list ,clause-test-var))
                          (length ,clause-test-var)
                          ,(not (null (member operator '(case typecase)))))))))
      `(progn
         ,(%def caselike)
         ,(%def ccaselike)
         ,(%def ecaselike)))))

(%define-caselikes (case ccase ecase))
(%define-caselikes (typecase ctypecase etypecase))
