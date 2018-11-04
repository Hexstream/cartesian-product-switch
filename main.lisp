(in-package #:cartesian-product-switch)

(defun %expand-linearize (testclauses env)
  (let (selection-forms selection-counts defaultps)
    (dolist (testclause testclauses (values (nreverse selection-forms)
                                            (nreverse selection-counts)
                                            (nreverse defaultps)))
      (multiple-value-bind (selform selcount defaultp)
          (%expand testclause env)
        (push selform selection-forms)
        (push selcount selection-counts)
        (push defaultp defaultps)))))

(defun %compute-factors (selection-counts)
  (let ((permutations-so-far 1))
    (map-bind (reduce) (((selection-count permutations-list) selection-counts)
                        (() :from-end t :initial-value nil))
      (prog1 (cons permutations-so-far permutations-list)
        (setf permutations-so-far
              (* selection-count permutations-so-far))))))

(defun %compute-selection-forms (testclauses env else-tag)
  (multiple-value-bind (selection-forms selection-counts defaultps)
      (%expand-linearize testclauses env)
    (values
     (map-bind (mapcar) ((selection-form selection-forms)
                         (defaultp defaultps)
                         (factor (%compute-factors selection-counts)))
       `(* ,factor ,(if defaultp
                        `(or ,selection-form (go ,else-tag))
                        selection-form)))
     selection-counts)))


(defmacro cartesian-product-switch ((&rest testclauses) &body clauses
                                    &environment env)
  (multiple-value-bind (forms else-clause)
      (let ((last (first (last clauses))))
        (if (typep last '(cons (member t otherwise)))
            (values (butlast clauses) last)
            (values clauses nil)))
    (let ((block-name (gensym (string '#:cartesian-product-switch)))
          (else-tag (gensym (string '#:else))))
      (multiple-value-bind (selection-forms selection-counts)
          (%compute-selection-forms testclauses env else-tag)
        (let ((clauses-count (length forms))
              (permutations-count (reduce #'* selection-counts)))
          (when (/= clauses-count permutations-count)
            (error "~S needs ~S = ~D normal-clause~:P but ~D provided."
                   'cartesian-product-switch
                   `(* ,@selection-counts)
                   permutations-count
                   clauses-count)))
        (let ((output-clauses (let ((index -1))
                                (map-bind (mapcar) ((form forms))
                                  `(,(incf index) ,form)))))
          `(block ,block-name
             (tagbody
                (return-from ,block-name
                  ,(if output-clauses
                       `(ecase (+ ,@selection-forms)
                          ,@output-clauses)
                       `(prog1 nil (+ ,@selection-forms))))
                ,else-tag
                (return-from ,block-name (progn ,@(rest else-clause))))))))))
