(asdf:defsystem #:cartesian-product-switch

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "CARTESIAN-PRODUCT-SWITCH is a macro for choosing the appropriate form to execute according to the combined results of multiple tests. This is a straightforward and efficient alternative to the convoluted ad-hoc conditionals one might otherwise resort to."

  :depends-on ("map-bind")

  :version "2.0"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "definitions")
	       (:file "main")))
