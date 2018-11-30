(cl:defpackage #:cartesian-product-switch
  (:nicknames #:cp-switch)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:import-from #:definitions-systems #:define)
  (:import-from #:trivial-jumptables #:ejumpcase)
  (:export #:cartesian-product-switch ; import this single symbol for normal usage.

           #:testclause))
