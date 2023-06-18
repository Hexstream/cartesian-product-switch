(cl:defpackage #:cartesian-product-switch
  (:nicknames #:cp-switch)
  (:use #:cl)
  (:import-from #:definitions-systems #:define)
  (:import-from #:trivial-jumptables #:ejumpcase)
  (:export #:cartesian-product-switch ; import this single symbol for normal usage.

           #:testclause
           #:testclause-kind
           #:standard-testclause-kind))
