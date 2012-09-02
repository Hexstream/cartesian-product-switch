(cl:defpackage #:cartesian-product-switch
  (:nicknames #:cp-switch)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:export #:cartesian-product-switch ; import this single symbol for normal usage.

           #:info
           #:standard-info
           #:name
           #:lambda-list
           #:expander
           #:locate
           #:expand
           #:ensure
           #:define))
