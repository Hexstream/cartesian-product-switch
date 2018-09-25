(cl:defpackage #:cartesian-product-switch_tests
  (:use #:cl #:parachute)
  (:import-from #:cartesian-product-switch #:cartesian-product-switch))

(cl:in-package #:cartesian-product-switch_tests)

(define-test "featured-example"
  (flet ((test (manner direction expected)
           (flet ((invoke ()
                    (cartesian-product-switch ((case manner
                                                 :walk
                                                 :flip)
                                               (case direction
                                                 :forward
                                                 :in-place
                                                 :backward))
                      "Walking..."
                      (error "Walking in-place is too boring!")
                      "Awkwardly walking backwards..."
                      "Frontflip!"
                      "Jumping in-place."
                      "Backflip!"
                      (t (error "Don't know how to ~A while going ~A." manner direction)))))
             (etypecase expected
               (string (is string= expected (invoke)))
               ((eql error) (fail (invoke)))))))
    (test :walk :forward "Walking...")
    (test :walk :in-place 'error)
    (test :walk :backward "Awkwardly walking backwards...")
    (test :flip :forward "Frontflip!")
    (test :flip :in-place "Jumping in-place.")
    (test :flip :backward "Backflip!")
    (test :crawl :forward 'error)
    (test :flip :sideways 'error)))

;;; TODO: Test other testclause kinds, and custom testclause kind definition.
