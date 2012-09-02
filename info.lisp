(in-package #:cartesian-product-switch)

(defvar *infos* (make-hash-table :test 'eq))

(deftype testclause-name ()
  '(and symbol (not null)))

(defgeneric cartesian-product-switch:name (object))
(defgeneric cartesian-product-switch:lambda-list (object))
(defgeneric cartesian-product-switch:expander (object))

(defclass cartesian-product-switch:info () ())

(defclass cartesian-product-switch:standard-info (info)
  ((%name :initarg :name
          :reader cartesian-product-switch:name
          :type testclause-name)
   (%lambda-list :initarg :lambda-list
                 :reader cartesian-product-switch:lambda-list
                 :type list)
   (%expander :initarg :expander
              :reader cartesian-product-switch:expander
              :type (or function symbol))))

(defun cartesian-product-switch:locate (testclause-name &key (errorp t))
  (check-type testclause-name testclause-name)
  (or (gethash testclause-name *infos*)
      (and errorp
           (error "No testclause kind with name ~S." testclause-name))))

(defun (setf %locate) (new testclause-name &key (errorp t))
  (declare (ignore errorp))
  (check-type testclause-name testclause-name)
  (check-type new info)
  (setf (gethash testclause-name *infos*) new))

(defun cartesian-product-switch:expand (testclause &optional env)
  (funcall (cartesian-product-switch:expander
            (cartesian-product-switch:locate (first testclause)))
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

(defun %remove-keys (keys plist)
  (let ((keys (if (listp keys) keys (list keys)))
        (processp nil))
    (map-bind (mapcan) ((key plist) (value (cdr plist)))
      (when (setf processp (not processp))
        (unless (member key keys)
          (list key value))))))

(defun cartesian-product-switch:ensure
    (name lambda-list expander
     &rest keys &key (class 'cartesian-product-switch:standard-info)
     &allow-other-keys)
  (setf (%locate name)
        (apply #'make-instance class
               :name name
               :lambda-list lambda-list
               :expander expander
               (%remove-keys :class keys))))

(defmacro cartesian-product-switch:define (name macro-lambda-list &body body)
  `(cartesian-product-switch:ensure ',name
                                    ',macro-lambda-list
                                    ,(%make-expander name macro-lambda-list body)))
