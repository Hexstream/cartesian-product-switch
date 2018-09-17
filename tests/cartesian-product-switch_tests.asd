(asdf:defsystem #:cartesian-product-switch_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "CARTESIAN-PRODUCT-SWITCH unit tests."

  :depends-on ("cartesian-product-switch"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:cartesian-product-switch_tests)))
