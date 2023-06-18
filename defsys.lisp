(in-package #:cartesian-product-switch)

(define (defsys:standard-system cp-switch:testclause-kind))

(defgeneric lambda-list (object))
(defgeneric expander (object))

(defclass cp-switch:standard-testclause-kind (cp-switch:testclause-kind defsys:standard-definition)
  ((%lambda-list :initarg :lambda-list
                 :reader lambda-list
                 :type list)
   (%expander :initarg :expander
              :reader expander
              :type (or function symbol))))

(defun %expand (testclause &optional env)
  (funcall (expander (defsys:locate 'cp-switch:testclause-kind (first testclause)))
           testclause
           env))

;; Not robust in the face of misplaced &environment.
(defun %extract-&environment (macro-lambda-list)
  (let ((tail (member '&environment macro-lambda-list)))
    (cond (tail
           (when (member '&environment tail)
             (error "More than one ~S parameter in ~S."
                    '&environment macro-lambda-list))
           (values (second tail)
                   (append (ldiff macro-lambda-list tail)
                           (cddr tail))))
          (t (values nil macro-lambda-list nil)))))

(defun %check-expected-operator (actual expected)
  (unless (eq actual expected)
    (error "Wrong operator ~S, expected ~S." actual expected)))

(defun %make-expander (name macro-lambda-list body)
  (let ((testclause-var (gensym (string '#:testclause)))
        (operator-var (gensym (string '#:operator))))
    (multiple-value-bind (env-var ordinary-lambda-list maybe-ignore-env)
        (multiple-value-bind (env-var ordinary-lambda-list envp)
            (%extract-&environment macro-lambda-list)
          (setf env-var (or env-var (gensym (string '#:env))))
          (values env-var
                  ordinary-lambda-list
                  (and (not envp) `((declare (ignore ,env-var))))))
      `(lambda (,testclause-var ,env-var)
         ,@maybe-ignore-env
         (let ((,operator-var (first ,testclause-var)))
           (destructuring-bind ,ordinary-lambda-list (rest ,testclause-var)
             (%check-expected-operator ,operator-var ',name)
             ,@body))))))

(defmethod defsys:expand ((prototype cp-switch:standard-testclause-kind) name environment args &rest options)
  (destructuring-bind (macro-lambda-list &body body) args
    (apply #'call-next-method prototype name environment
           `(:lambda-list ',macro-lambda-list
             :expander ,(%make-expander name macro-lambda-list body))
           options)))
